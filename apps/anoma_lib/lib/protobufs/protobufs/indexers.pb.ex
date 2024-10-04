defmodule Protobufs.Indexers.Service do
  @moduledoc false

  use GRPC.Service,
    name: "Protobufs.Indexers",
    protoc_gen_elixir_version: "0.13.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      name: "Indexers",
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          name: "ListNullifiers",
          input_type: ".Protobufs.Indexer.Nullifiers.Request",
          output_type: ".Protobufs.Indexer.Nullifiers.Response",
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

  rpc(
    :ListNullifiers,
    Protobufs.Indexer.Nullifiers.Request,
    Protobufs.Indexer.Nullifiers.Response
  )
end

defmodule Protobufs.Indexers.Stub do
  @moduledoc false

  use GRPC.Stub, service: Protobufs.Indexers.Service
end
