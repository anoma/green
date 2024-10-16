defmodule Anoma.Node.Supervisor do
  @moduledoc """
  I am the top level supervisor for the Anoma node.
  """

  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_args) do
    children = [
      Anoma.Node.Transaction.Supervisor,
      Anoma.Node.Transport.Supervisor,
      Anoma.Node.Utility.Supervisor
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
