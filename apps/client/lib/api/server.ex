defmodule Client.Api.Server do
  alias Protobufs.IntentPool.ListIntents.Request
  alias Protobufs.IntentPool.ListIntents.Response
  alias Protobufs.Intents

  use GRPC.Server, service: Intents.Service

  @spec list_intents(Request.t, GRPC.Server.Stream.t) :: Response.t
  def list_intents(request, _stream) do
    %Response{intents: ["intent1", "intent2"]}
  end
end
