defmodule Client.Examples.EClient do
  alias Anoma.Node.Examples.ENode
  alias Anoma.Node.Examples.ETransport.ETcp
  alias Anoma.Crypto.Id

  use Example

  @doc """
  I setup a basic node that is used to interact with.

  Any other running ENode works as well.
  """
  @spec setup_node(Id.t()) :: ENode.t()
  def setup_node(node_id \\ Examples.ECrypto.londo()) do
    node = ENode.start_node(node_id)
    ETcp.create_tcp_listener(node)
  end

  @doc """
  I list the intents over grpc on the client.
  """
  def list_intents(_enode) do
    alias Protobufs.IntentPool.ListIntents.Request
    # alias Protobufs.IntentPool.ListIntents.Response
    alias Protobufs.NodeInfo

    {:ok, channel} = GRPC.Stub.connect("localhost:50051")
    node_id = Id.new_keypair()

    node_info = %NodeInfo{
      sign: node_id.external.sign,
      encrypt: node_id.external.encrypt
    }

    request = %Request{sender_info: node_info}
    {:ok, _reply} = channel |> Protobufs.Intents.Stub.list_intents(request)
  end

  @doc """
  I add an intent to the client.
  """
  def add_intent() do
    alias Protobufs.IntentPool.AddIntent.Request
    # alias Protobufs.IntentPool.AddIntent.Response
    alias Protobufs.NodeInfo

    {:ok, channel} = GRPC.Stub.connect("localhost:50051")
    node_id = Anoma.Crypto.Id.new_keypair()

    node_info = %NodeInfo{
      sign: node_id.external.sign,
      encrypt: node_id.external.encrypt
    }

    request = %Request{
      sender_info: node_info,
      intent:
        "this is my intent. there are many like it, but this one is mine."
    }

    {:ok, _reply} = channel |> Protobufs.Intents.Stub.add_intent(request)
  end

  @doc """
  I list all nullifiers.
  """
  def list_nullifiers() do
    alias Protobufs.Indexer.Nullifiers.Request
    # alias Protobufs.Indexer.Nullifiers.Response
    alias Protobufs.NodeInfo

    {:ok, channel} = GRPC.Stub.connect("localhost:50051")
    node_id = Anoma.Crypto.Id.new_keypair()

    node_info = %NodeInfo{
      sign: node_id.external.sign,
      encrypt: node_id.external.encrypt
    }

    request = %Request{sender_info: node_info}

    {:ok, _reply} = channel |> Protobufs.Intents.Stub.list_nullifiers(request)
  end

  @doc """
  I list all unrevealed commits.
  """
  def list_unrevealed_commits() do
    alias Protobufs.Indexer.UnrevealedCommits.Request
    # alias Protobufs.Indexer.UnrevealedCommits.Response
    alias Protobufs.NodeInfo

    {:ok, channel} = GRPC.Stub.connect("localhost:50051")
    node_id = Anoma.Crypto.Id.new_keypair()

    node_info = %NodeInfo{
      sign: node_id.external.sign,
      encrypt: node_id.external.encrypt
    }

    request = %Request{sender_info: node_info}

    {:ok, _reply} =
      channel |> Protobufs.Intents.Stub.list_unrevealed_commits(request)
  end

  @doc """
  I list all unspent resources.
  """
  def list_unspent_resources() do
    alias Protobufs.Indexer.UnspentResources.Request
    # alias Protobufs.Indexer.UnspentResources.Response
    alias Protobufs.NodeInfo

    {:ok, channel} = GRPC.Stub.connect("localhost:50051")
    node_id = Anoma.Crypto.Id.new_keypair()

    node_info = %NodeInfo{
      sign: node_id.external.sign,
      encrypt: node_id.external.encrypt
    }

    request = %Request{sender_info: node_info}

    {:ok, _reply} =
      channel |> Protobufs.Intents.Stub.list_unspent_resources(request)
  end

  @doc """
  I prove something using the client.
  """
  def prove_something() do
    alias Protobufs.Prove.Request
    # alias Protobufs.Prove.Response
    alias Protobufs.NodeInfo

    {:ok, channel} = GRPC.Stub.connect("localhost:50051")
    node_id = Anoma.Crypto.Id.new_keypair()

    node_info = %NodeInfo{
      sign: node_id.external.sign,
      encrypt: node_id.external.encrypt
    }

    request = %Request{sender_info: node_info, intent: "prove this, please"}

    {:ok, _reply} =
      channel |> Protobufs.Intents.Stub.prove(request)
  end
end
