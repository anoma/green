defmodule Protobufs.Indexer.Nullifiers do
  @moduledoc false

  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "Nullifiers",
      field: [],
      nested_type: [
        %Google.Protobuf.DescriptorProto{
          name: "Request",
          field: [
            %Google.Protobuf.FieldDescriptorProto{
              name: "sender_info",
              extendee: nil,
              number: 1,
              label: :LABEL_OPTIONAL,
              type: :TYPE_MESSAGE,
              type_name: ".Protobufs.NodeInfo",
              default_value: nil,
              options: nil,
              oneof_index: nil,
              json_name: "senderInfo",
              proto3_optional: nil,
              __unknown_fields__: []
            }
          ],
          nested_type: [],
          enum_type: [],
          extension_range: [],
          extension: [],
          options: nil,
          oneof_decl: [],
          reserved_range: [],
          reserved_name: [],
          __unknown_fields__: []
        },
        %Google.Protobuf.DescriptorProto{
          name: "Response",
          field: [
            %Google.Protobuf.FieldDescriptorProto{
              name: "nullifiers",
              extendee: nil,
              number: 1,
              label: :LABEL_REPEATED,
              type: :TYPE_STRING,
              type_name: nil,
              default_value: nil,
              options: nil,
              oneof_index: nil,
              json_name: "nullifiers",
              proto3_optional: nil,
              __unknown_fields__: []
            }
          ],
          nested_type: [],
          enum_type: [],
          extension_range: [],
          extension: [],
          options: nil,
          oneof_decl: [],
          reserved_range: [],
          reserved_name: [],
          __unknown_fields__: []
        }
      ],
      enum_type: [],
      extension_range: [],
      extension: [],
      options: nil,
      oneof_decl: [],
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end
end
