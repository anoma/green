defmodule Anoma.Node.Executor.Worker do
  @moduledoc """
  I am the Worker Engine.

  My instance gets launched by the Executor and is connected to a unique
  transaction.

  I am responsible for the main work done to run a successful transaction
  lifecycle. This includes processing of transactions, calling for their
  ordering via the Ordering Engine, creation and revision of commitment
  trees, nullifier key-checking, as well as the storage of relevant data
  for transaction completion before block-execution.

  ### Public API

  I provide the following public functionality:

  - `rm_nullifier_check/2`
  """

  alias Anoma.Resource.Transaction, as: TTransaction
  alias Anoma.RM.Transaction
  alias Anoma.ShieldedResource.ShieldedTransaction
  alias Anoma.Node.{Storage, Ordering, Logger, Router}
  alias __MODULE__

  use Router.Engine, restart: :temporary
  use TypedStruct

  import Nock
  require Noun

  @type backend() :: :kv | :rm | :cairo | :ro

  @type transaction() :: {backend(), Noun.t() | binary()}

  typedstruct do
    @typedoc """
    I am the type of a Worker Engine instance.

    I contain all the info for appropriate transaction processing.

    ### Fields

    - `:id` - The ID of the transaction fed in.
    - `:tx` - The transaction code.
    - `:env` - The environment for the transaction to be evaluated in. E.g.
               contains the Ordering engine address. See `Nock.t()`
    - `:completion_topic` - The address of the topic connected to the
                            relevant Executor Engine for broadcasting.
    - `:reply_to` - The address that the computed value (for read-only
                    transaction) is sent to.
    """

    field(:id, non_neg_integer())
    field(:tx, transaction())
    field(:env, Nock.t())
    field(:completion_topic, Router.Addr.t())
    field(:reply_to, Router.addr())
  end

  @doc """
  I am the Worker initialization function.

  I send myself a `:run` message which launches my core functionality and
  return the appropriate state.

  ### Pattern-Matching Variations

  - `init({id, tx, env, completion_topic, reply_to})` - I receive a tuple with all
                                                        the specified info to launch
                                                        a Worker instance.
  """

  @spec init(
          {non_neg_integer(), transaction(), Nock.t(), Router.Addr.t(),
           Router.addr() | nil}
        ) ::
          {:ok, Worker.t()}
  def init({id, tx, env, completion_topic, reply_to}) do
    send(self(), :run)

    {:ok,
     %__MODULE__{
       id: id,
       tx: tx,
       env: env,
       completion_topic: completion_topic,
       reply_to: reply_to
     }}
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  def handle_info(:run, s) do
    result = run(s)

    Router.cast(
      s.completion_topic,
      {:worker_done, Router.self_addr(), result}
    )

    {:stop, :normal, s}
  end

  ############################################################
  #                  Genserver Implementation                #
  ############################################################

  @spec run(t()) :: :ok | :error
  defp run(s = %__MODULE__{tx: {backend, tx}, env: env})
       when Noun.is_noun_atom(tx) do
    case Nock.Cue.cue(tx) do
      {:ok, tx} ->
        run(%__MODULE__{s | tx: {backend, tx}})

      :error ->
        storage = Router.Engine.get_state(env.ordering).storage
        log_info({:fail, "failed to cue!", env.logger})
        wait_for_ready(s)
        snapshot(storage, env)
        :error
    end
  end

  defp run(s = %__MODULE__{tx: {:ro, _}}) do
    execute_key_value_tx(s, &send_value/3)
  end

  defp run(s = %__MODULE__{tx: {:kv, _}}) do
    execute_key_value_tx(s, &store_value/3)
  end

  defp run(s = %__MODULE__{tx: {:rm, _}}) do
    execute_rm_tx(s, TTransaction)
  end

  defp run(s = %__MODULE__{tx: {:cairo, _}}) do
    execute_rm_tx(s, ShieldedTransaction)
  end

  @spec execute_key_value_tx(t(), fun()) :: :ok | :error
  defp execute_key_value_tx(
         s = %__MODULE__{
           id: id,
           tx: {_, proto_tx},
           env: env,
           completion_topic: topic,
           reply_to: reply_to
         },
         process
       ) do
    logger = env.logger

    log_info({:dispatch, id, logger})
    storage = Router.Engine.get_state(env.ordering).storage

    with {:ok, stage_2_tx} <- nock(proto_tx, [9, 2, 0 | 1], env),
         {:ok, ordered_tx} <-
           nock(stage_2_tx, [10, [6, 1 | id], 0 | 1], env),
         {:ok, result} <- nock(ordered_tx, [9, 2, 0 | 1], env),
         :ok <-
           process.(s, result, storage) do
      snapshot(storage, env)
      log_info({:success_run, logger})
      :ok
    else
      e ->
        log_info({:fail, e, logger})

        # notify the topic and reply-to address about the error
        err_msg = {:worker_error}
        send_if_addr(reply_to, err_msg)
        Router.cast(topic, err_msg)

        wait_for_ready(s)
        snapshot(storage, env)
        :error
    end
  end

  @spec send_value(t(), Noun.t(), Router.addr()) :: :ok | nil
  defp send_value(
         %__MODULE__{
           tx: {:ro, _},
           env: env,
           completion_topic: topic,
           reply_to: reply_to
         },
         value,
         _storage
       ) do
    # send the value to reply-to address and the topic
    reply_msg = {:read_value, value}
    send_if_addr(reply_to, reply_msg)
    Router.cast(topic, reply_msg)

    log_info({:get, value, env.logger})
  end

  @spec store_value(t(), Noun.t(), Router.addr()) :: any()
  defp store_value(
         s = %__MODULE__{tx: {:kv, _}, env: env},
         key_value,
         storage
       ) do
    with [key | value] <- key_value do
      true_order = wait_for_ready(s)

      logger = env.logger
      log_info({:writing, true_order, logger})
      Storage.put(storage, key, value)
      log_info({:put, key, logger})
      :ok
    else
      e -> e
    end
  end

  # Must be passed a from noun
  @spec execute_rm_tx(t(), module) :: :ok | :error
  defp execute_rm_tx(s = %__MODULE__{id: id, tx: {_, gate}, env: env}, mod) do
    logger = env.logger

    log_info({:dispatch, id, logger})
    storage = Router.Engine.get_state(env.ordering).storage

    with {:ok, ordered_tx} <- nock(gate, [10, [6, 1 | id], 0 | 1], env),
         {:ok, resource_tx} <- nock(ordered_tx, [9, 2, 0 | 1], env),
         {:ok, vm_resource_tx} <- mod.from_noun(resource_tx),
         true_order = wait_for_ready(s),
         true <- Transaction.verify(vm_resource_tx),
         # TODO: add root existence check. The roots must be traceable
         # in historical records.
         true <-
           rm_nullifier_check(
             storage,
             Transaction.storage_nullifiers(vm_resource_tx)
           ) do
      persist(env, true_order, vm_resource_tx)
    else
      # The failure had to be on the true match above, which is after
      # the wait for ready
      false ->
        log_info({:fail, false, logger})
        snapshot(storage, env)
        :error

      # This failed before the waiting for read as it's likely :error
      e ->
        log_info({:fail, e, logger})
        wait_for_ready(s)
        snapshot(storage, env)
        :error
    end
  end

  @spec persist(Nock.t(), Noun.t(), Transaction.t()) :: any()
  defp persist(env, true_order, vm_resource_tx) do
    logger = env.logger
    storage = Router.Engine.get_state(env.ordering).storage

    log_info({:writing, true_order, logger})
    # this is not quite correct, but storage has to be revisited as a whole
    # for it to be made correct.
    # in particular, the get/put api must be deleted, since it cannot be correct,
    # but an append api should also be added.
    # the latter requires the merkle tree to be complete
    cm_tree = Transaction.cm_tree(vm_resource_tx, storage)

    commitments = Transaction.storage_commitments(vm_resource_tx)

    for commitment <- commitments do
      cm_key = ["rm", "commitments", commitment]

      Storage.put(storage, cm_key, true)
      log_info({:put, cm_key, logger})
    end

    CommitmentTree.add(cm_tree, commitments)

    Storage.put(storage, ["rm", "commitment_root"], cm_tree.root)

    for nullifier <- Transaction.storage_nullifiers(vm_resource_tx) do
      nf_key = ["rm", "nullifiers", nullifier]
      Storage.put(storage, nf_key, true)
      log_info({:put, nf_key, logger})
    end

    snapshot(storage, env)
    log_info({:success_run, logger})
    :ok
  end

  ############################################################
  #                     Conceptual Helpers                   #
  ############################################################

  @doc """
  I perform the nullifier check for a resource machine transaction.

  Given a storage and a list of stored nullifiers I check their placing in storage.
  """
  @spec rm_nullifier_check(Router.addr(), list(binary())) :: bool()
  def rm_nullifier_check(storage, nullifiers) do
    for nullifier <- nullifiers, reduce: true do
      acc ->
        nf_key = ["rm", "nullifiers", nullifier]
        acc && Storage.get(storage, nf_key) == :absent
    end
  end

  @spec wait_for_ready(t()) :: any()
  defp wait_for_ready(%__MODULE__{env: env, id: id}) do
    logger = env.logger

    log_info({:ensure_read, logger})

    Ordering.caller_blocking_read_id(
      env.ordering,
      [id | env.snapshot_path]
    )

    log_info({:waiting_write_ready, logger})

    receive do
      {:write_ready, id} ->
        log_info({:write_ready, logger})
        id
    end
  end

  @spec snapshot(Router.addr(), Nock.t()) ::
          :ok | nil
  defp snapshot(storage, env) do
    snapshot = hd(env.snapshot_path)
    log_info({:snap, {storage, snapshot}, env.logger})
    Storage.put_snapshot(storage, snapshot)
  end

  @spec send_if_addr(Router.addr() | nil, any()) :: :ok | nil
  defp send_if_addr(addr, msg) do
    if addr do
      Router.cast(addr, msg)
    end
  end

  ############################################################
  #                     Logging Info                         #
  ############################################################

  defp log_info({:dispatch, id, logger}) do
    Logger.add(logger, :info, "Worker dispatched.
    Order id: #{inspect(id)}")
  end

  defp log_info({:writing, order, logger}) do
    Logger.add(logger, :info, "Worker writing.
    True order: #{inspect(order)}")
  end

  defp log_info({:fail, error, logger}) do
    Logger.add(logger, :error, "Worker failed! #{inspect(error)}")
  end

  defp log_info({:get, value, logger}) do
    Logger.add(logger, :info, "Getting value #{inspect(value)}")
  end

  defp log_info({:put, key, logger}) do
    Logger.add(logger, :info, "Putting #{inspect(key)}")
  end

  defp log_info({:success_run, logger}) do
    Logger.add(logger, :info, "Run successful!")
  end

  defp log_info({:ensure_read, logger}) do
    Logger.add(
      logger,
      :info,
      "#{inspect(self())}: making sure the snapshot is ready"
    )
  end

  defp log_info({:waiting_write_ready, logger}) do
    Logger.add(
      logger,
      :info,
      "#{inspect(self())}: waiting for a write ready"
    )
  end

  defp log_info({:write_ready, logger}) do
    Logger.add(
      logger,
      :info,
      "#{inspect(self())}: write ready"
    )
  end

  defp log_info({:snap, {s, ss}, logger}) do
    Logger.add(
      logger,
      :info,
      "Taking snapshot key #{inspect(ss)} in storage #{inspect(s)}"
    )
  end
end
