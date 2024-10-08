defmodule Anoma.Node.Registry do
  use TypedStruct

  alias Anoma.Crypto.Id

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct enforce: true, module: Address do
    field(:node_id, Id.t())
    field(:engine, atom())
    field(:label, atom(), default: nil)
  end

  defimpl Inspect, for: Address do
    def inspect(address, _opts) do
      addr = :erlang.phash2(address.node_id)
      engine = address.engine
      label = address.label
      "#{engine}:#{addr}#{if label, do: ":#{label}", else: ""}"
    end
  end

  ############################################################
  #                      Public API                          #
  ############################################################

  @doc """
  I register the calling process under the name of the engine at the given node id.
  """
  @spec register(Id.t(), atom(), atom()) :: {:ok, pid} | {:error, term}
  def register(node_id, engine, label \\ nil) do
    address = address(node_id, engine, label)
    Elixir.Registry.register(__MODULE__, address, nil)
  end

  @doc """
  I create the address for the engine at the given node id.
  """
  @spec address(Id.t(), atom()) :: Address.t()
  def address(node_id, engine, label \\ nil) do
    %Address{node_id: node_id, engine: engine, label: label}
  end

  @doc """
  I generate the name for a process with the given node_id and engine name.
  """
  @spec name(Id.t(), atom(), atom()) ::
          {:via, Registry, {atom(), Address.t()}}
  def name(node_id, engine, label \\ nil) do
    address = %Address{node_id: node_id, engine: engine, label: label}
    {:via, Registry, {__MODULE__, address, inspect(address)}}
  end

  @doc """
  Given a node id and engine, I check if there is a process registered with that address
  and return the pid.
  """
  @spec whereis(Id.t(), atom(), atom()) :: pid() | nil
  def whereis(node_id, engine, label \\ nil) do
    address = %Address{node_id: node_id, engine: engine, label: label}

    case Registry.lookup(__MODULE__, address) do
      [{pid, _}] -> pid
      [] -> nil
    end
  end

  @doc """
  Given a node_id, I return a list of all the engine names I know for this node.

  I filter out all the non-engine names based on an allow list.
  """
  @spec engines_for(Id.t()) :: [atom()]
  def engines_for(node_id) do
    pattern = {%{node_id: :"$1", engine: :"$2"}, :"$3", :"$4"}

    # guards: filters applied on the results

    guards = [{:==, :"$1", node_id}]

    # shape: the shape of the results the registry should return
    shape = [:"$2"]

    Registry.select(__MODULE__, [{pattern, guards, shape}])
    |> Enum.filter(
      &(&1 in [
          Anoma.Node.Transaction.Mempool,
          Anoma.Node.Transaction.IntentPool,
          Client
        ])
    )
    |> Enum.sort()
  end

  @doc """
  I return the contents of the registry.
  """
  @spec dump_register() :: [{Address.t(), pid(), any()}]
  def dump_register() do
    Registry.select(__MODULE__, [
      {{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}
    ])
    |> Enum.sort()
  end
end
