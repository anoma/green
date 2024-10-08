defmodule Client.GRPCProxy do
  use GenServer
  use TypedStruct

  alias Protobufs.IntentPool.ListIntents
  alias Protobufs.IntentPool.AddIntent

  ############################################################
  #                    State                                 #
  ############################################################

  typedstruct do
    @typedoc """
    I am the state of a TCP listener.

    My fields contain information to listen for TCP connection with a remote node.

    ### Fields
    - `:node_port` - The port on which the remote node is listening to GRPC.
    - `:node_host` - The host on which the remote node is listening to GRPC.
    - `:channel`   - The channel to the remote grpc.
    """
    field(:node_port, integer())
    field(:node_host, String.t())
    field(:channel, any())
  end

  ############################################################
  #                    Genserver Helpers                     #
  ############################################################

  def start_link(args) do
    args = Keyword.validate!(args, [:node_port, :node_host])
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(args) do
    state = struct(__MODULE__, Enum.into(args, %{}))
    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state) do
    case GRPC.Stub.connect("#{state.node_host}:#{state.node_port}") do
      {:ok, channel} ->
        {:noreply, %{state | channel: channel}}

      _ ->
        {:stop, :error}
    end
  end

  ############################################################
  #                      Public RPC API                      #
  ############################################################

  def list_intents(sender_info) do
    GenServer.call(__MODULE__, {:list_intents, sender_info})
  end

  def add_intent(sender_info, intent) do
    GenServer.call(__MODULE__, {:add_intent, sender_info, intent})
  end

  ############################################################
  #                    Genserver Behavior                    #
  ############################################################

  @impl true
  def handle_call({:list_intents, sender_info}, _from, state) do
    request = %ListIntents.Request{sender_info: sender_info}
    intents = Protobufs.Intents.Stub.list_intents(state.channel, request)
    {:reply, intents, state}
  end

  def handle_call({:add_intent, sender_info, intent}, _from, state) do
    request = %AddIntent.Request{sender_info: sender_info, intent: intent}
    result = Protobufs.Intents.Stub.add_intent(state.channel, request)
    {:reply, result, state}
  end

  @impl true
  def handle_cast(_message, state) do
    {:noreply, state}
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end
end
