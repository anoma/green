defmodule Anoma.Node.Examples.EConsensus do
  alias Anoma.Node.Utility.Consensus
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Node.Examples.{ETransaction, ELogging}
  alias Anoma.Node.Registry
  alias Anoma.Crypto.Id

  require EventBroker.Event

  require ExUnit.Assertions
  import ExUnit.Assertions

  def restart_consensus(node_id \\ Examples.ECrypto.londo()) do
    stop_consensus(node_id)

    Consensus.start_link(node_id: node_id, interval: 100)
  end

  def stop_consensus(node_id \\ Examples.ECrypto.londo()) do
    pid = Registry.whereis(node_id, Consensus)

    if pid && Process.alive?(pid) do
      GenServer.stop(pid)
    end
  end

  def restart_consensus_env(node_id \\ Examples.ECrypto.londo()) do
    ETransaction.restart_tx_module(node_id)
    ELogging.restart_logging()
    restart_consensus(node_id)
  end

  def startup_execution(node_id \\ Id.new_keypair()) do
    EventBroker.subscribe_me([])
    restart_consensus_env(node_id)

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.BlockEvent{
          order: []
        }
      },
      5000
    )

    EventBroker.unsubscribe_me([])

    stop_consensus(node_id)
  end

  def execution_continues(node_id \\ Id.new_keypair()) do
    EventBroker.subscribe_me([])
    restart_consensus_env(node_id)

    Mempool.tx(node_id, ETransaction.zero(), "id 1")

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.ConsensusEvent{
          order: []
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.BlockEvent{
          order: []
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.ConsensusEvent{
          order: ["id 1"]
        }
      },
      5000
    )

    assert_receive(
      %EventBroker.Event{
        body: %Mempool.BlockEvent{
          order: ["id 1"]
        }
      },
      5000
    )

    stop_consensus(node_id)
  end
end
