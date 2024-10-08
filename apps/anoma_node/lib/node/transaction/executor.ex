defmodule Anoma.Node.Transaction.Executor do
  alias __MODULE__
  alias Anoma.Crypto.Id
  alias Anoma.Node.Transaction.{Backends, Mempool, Ordering}
  alias Anoma.Node.Registry

  use TypedStruct

  use GenServer

  require EventBroker.Event

  typedstruct do
    field(:node_id, Id.t())
  end

  typedstruct enforce: true, module: ExecutionEvent do
    field(:result, list({:ok | :error, binary()}))
  end

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id])
    name = Registry.name(args[:node_id], __MODULE__)
    GenServer.start_link(__MODULE__, args, name: name)
  end

  @spec init(any()) :: {:ok, Executor.t()}
  def init(args) do
    Process.set_label(__MODULE__)

    EventBroker.subscribe_me([
      Mempool.worker_module_filter(),
      complete_filter()
    ])

    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state}
  end

  def launch(node_id, tw_w_backend, id) do
    pid = Registry.whereis(node_id, __MODULE__)
    GenServer.cast(pid, {:launch, tw_w_backend, id})
  end

  def execute(node_id, consensus) do
    pid = Registry.whereis(node_id, __MODULE__)
    GenServer.cast(pid, {:execute, consensus})
  end

  def handle_cast({:launch, tw_w_backend, id}, state) do
    Task.start(fn ->
      Backends.execute(state.node_id, tw_w_backend, id)
    end)

    {:noreply, state}
  end

  def handle_cast({:execute, consensus}, state) do
    Ordering.order(state.node_id, consensus)

    res_list =
      for id <- consensus, reduce: [] do
        list ->
          receive do
            %EventBroker.Event{
              body: %Backends.CompleteEvent{
                tx_id: ^id,
                tx_result: res
              }
            } ->
              [{res, id} | list]
          after
            5000 -> raise "Timeout waiting for #{inspect(id)}"
          end
      end

    execution_event(res_list)
    {:noreply, state}
  end

  def execution_event(res_list) do
    event =
      EventBroker.Event.new_with_body(%__MODULE__.ExecutionEvent{
        result: res_list
      })

    EventBroker.event(event)
  end

  def complete_filter() do
    %Backends.CompleteFilter{}
  end
end
