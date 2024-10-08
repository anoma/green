defmodule Anoma.Node.Transport.Supervisor do
  @moduledoc """
  I am the transport supervisor.

  My main functionality is to supervise the physical transport connections
  in this node (e.g., TCP connections).
  """
  use Supervisor

  require Logger

  def start_link(args) do
    args = Keyword.validate!(args, [:node_id, ports: []])
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  @doc """
  I initialize a new transport supervision tree.

  ### Options

  - `:node_id` - The key of the local node.
  """
  def init(args) do
    Logger.debug("starting transport supervisor #{inspect(args)}")

    # validate args and set defaults
    args = Keyword.validate!(args, [:node_id, ports: []])
    node_id = args[:node_id]
    _ports = args[:ports]

    # tcp supervisor
    tcp_supervisor =
      {
        DynamicSupervisor,
        name: Anoma.Node.Registry.name(node_id, :tcp_supervisor),
        strategy: :one_for_one,
        max_restarts: 1_000_000,
        max_seconds: 1
      }

    # proxy supervisor
    proxy_supervisor =
      {DynamicSupervisor,
       name: Anoma.Node.Registry.name(node_id, :proxy_supervisor),
       strategy: :one_for_one,
       max_restarts: 1_000_000,
       max_seconds: 1}

    # start the supervisor
    children = [
      tcp_supervisor,
      proxy_supervisor
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
