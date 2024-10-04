defmodule Client do
  @moduledoc """
  Documentation for `Client`.
  """

  alias Client.Connection
  alias Anoma.Node.Registry
  alias Client.Connections
  alias Protobufs
  alias Anoma.Node.Transport.Messages
  alias Anoma.Node.Indexer
  alias Anoma.Node.Registry
  alias Anoma.Node.Transaction.Intentpool
  alias Anoma.Node.Transaction.Mempool
  alias Anoma.Crypto.Id

  def connect(host, port, node_id) do
    # configure the environment for the app
    {:ok, host} = resolve_host(host)

    # attempt to connect
    case try_connect(host, port) do
      {:ok, socket} ->
        # start a connection for the socket
        start_connection(socket, node_id)

        # register as a client engine
        Registry.register(node_id, Client)

        # connected, yay
        {:ok, node_id}

    end
  end

  def dump_mempool(node_id) do
    # generate the request to get intents
    message = Messages.call_to_proto(:dump, Mempool, node_id)
    GenServer.call(Connection, {:tcp_out, message})
  end

  def send_intent(node_id, intent) do
    message =
      Messages.call_to_proto({:add_intent, intent}, Intentpool, node_id)

    GenServer.call(Connection, {:tcp_out, message})
  end

  def get_intents(node_id) do
    message = Messages.call_to_proto(:list_intents, Intentpool, node_id)
    GenServer.call(Connection, {:tcp_out, message})
  end

  def nullifiers(node_id) do
    message = Messages.call_to_proto(:list_nullifiers, Indexer, node_id)
    GenServer.call(Connection, {:tcp_out, message})
  end

  def unrevealed_commitments(node_id) do
    message =
      Messages.call_to_proto(:list_unrevealed_commitments, Indexer, node_id)

    GenServer.call(Connection, {:tcp_out, message})
  end

  def unspent_resources(node_id) do
    message =
      Messages.call_to_proto(:list_unspent_resources, Indexer, node_id)

    GenServer.call(Connection, {:tcp_out, message})
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  # @doc """
  # I resolve a host in either string or charlist form into
  # a tuple.
  # """
  @spec resolve_host(host :: String.t() | list() | tuple()) ::
          {:ok, tuple()} | :error
  defp resolve_host(host) when is_binary(host) do
    host
    |> String.to_charlist()
    |> resolve_host()
  end

  defp resolve_host(host) when is_list(host) do
    case :inet.parse_address(host) do
      {:ok, ip} -> {:ok, ip}
      {:error, _} -> :inet.getaddr(host, :inet)
    end
  end

  defp resolve_host(host) when is_tuple(host), do: {:ok, host}

  # @doc """
  # I try to connect to the remote server.
  # I return the socket if it was successful.
  # """
  @spec try_connect(any(), any()) :: {:ok, :inet.socket()} | :error
  defp try_connect(host, port) do
    # attempt to conenct to the remote node
    socket_opts = [
      :binary,
      active: false,
      exit_on_close: true,
      reuseaddr: true,
      ifaddr: host
    ]

    :gen_tcp.connect(host, port, socket_opts)
  end

  # @doc """
  # I start a connection process for the given socket.
  # """
  @spec start_connection(:inet.socket(), Id.t()) ::
          {:ok, pid()} | {:error, term()}
  defp start_connection(socket, node_id) do
    args = [socket: socket, node_id: node_id]

    case DynamicSupervisor.start_child(Connections, {Connection, args}) do
      {:ok, pid} ->
        :gen_tcp.controlling_process(socket, pid)

        :inet.setopts(socket, [{:active, true}])

        {:ok, pid}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
