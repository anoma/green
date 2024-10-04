defmodule Protobufs.Intents.Service do
  @moduledoc false

  use GRPC.Service, name: "Protobufs.Intents", protoc_gen_elixir_version: "0.13.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      name: "Intents",
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          name: "ListIntents",
          input_type: ".Protobufs.IntentPool.ListIntents.Request",
          output_type: ".Protobufs.IntentPool.ListIntents.Response",
          options: %Google.Protobuf.MethodOptions{
            deprecated: false,
            idempotency_level: :IDEMPOTENCY_UNKNOWN,
            uninterpreted_option: [],
            __pb_extensions__: %{},
            __unknown_fields__: []
          },
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        }
      ],
      options: nil,
      __unknown_fields__: []
    }
  end

  rpc :ListIntents,
      Protobufs.IntentPool.ListIntents.Request,
      Protobufs.IntentPool.ListIntents.Response
end

defmodule Protobufs.Intents.Stub do
  @moduledoc false

  use GRPC.Stub, service: Protobufs.Intents.Service
end
