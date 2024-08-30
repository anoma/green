defmodule Anoma.Configuration do
  @moduledoc """
  I am a configuration module. I read the provided TOML configuration file
  and feed the appropriate info for Node launching

  The codebase has a corresponding file that can inform the user of the
  format I expect.

  ### Public API

  - `configuration/0`
  - `configuration/1`
  - `read_configuration/1`
  - `node_settings/1`
  - `save/1`
  - `save/2`
  - `locate_dump_file/1`
  - `launch_min/1`
  - `launch_min/2`
  """

  alias Anoma.System.Directories
  alias Anoma.Node

  ############################################################
  #                          Types                           #
  ############################################################

  @type section_format() :: [{String.t(), (term() -> boolean()), term()}]
  @type configuration() :: [{String.t(), section_format()}]

  @type section_map() :: %{String.t() => term()}
  @type configuration_map() :: %{String.t() => section_map()}

  @type node_settings() :: Keyword.t()

  @typedoc """
  I control options for `launch_min/2`

  ###Options

  - `:use_rocksdb` - see `t:Anoma.Node.configuration/0` for more
  information
  - `:supervisor` - This flag determine if we use a supervisor and if
  so what options. See `t:Supervisor.option/0 ` for supervisor options
  - `:testing` - This flag notes if we are testing the node. This gets
    fed directly into the type `t:Anoma.Node.configuration/0` for
    `Anoma.Node.start_link/1`. Please consult the
    `t:Anoma.Node.configuration/0` documentation for the full effect
    this has on the node
  """
  @type launch_option ::
          {:use_rocksdb, boolean()}
          | {:supervisor, [Supervisor.option()]}
          | {:testing, boolean()}

  ############################################################
  #                   Default Values                         #
  ############################################################

  @dump_format [
    {"dump", &is_binary/1, "anoma_#{Mix.env()}.dmp" |> Directories.data()}
  ]
  @node_format [
    {"block_storage", &is_binary/1, "anoma_block"},
    {"name", &is_binary/1, "anoma"},
    {"order", &is_binary/1, "Anoma.Order"},
    {"ping_time", &__MODULE__.is_pinger/1,
     if(Mix.env() == :prod, do: 10000, else: "no_timer")},
    {"qualified", &is_binary/1, "Anoma.Qualified"},
    {"logger_table", &is_binary/1, "Anoma.Logger"},
    {"snapshot_path", &is_binary/1, "my_special_nock_snapshot"}
  ]
  @configuration_format [{"dump", @dump_format}, {"node", @node_format}]

  @spec configuration_format() :: configuration
  def configuration_format(), do: @configuration_format

  @spec default_configuration_location() :: Path.t()
  def default_configuration_location(env \\ Mix.env()) do
    Directories.configuration("/anoma_#{env}.toml")
  end

  def default_data_location(env \\ Mix.env()) do
    Directories.data("anoma_#{env}.dmp")
  end

  ############################################################
  #                  Main Functionality                      #
  ############################################################

  @spec configuration(configuration_map()) :: configuration_map()
  @spec configuration() :: configuration_map()
  def configuration(user_map \\ %{}) do
    Enum.reduce(configuration_format(), %{}, fn {key, section}, config ->
      user_section = Map.get(user_map, key, %{})
      Map.put(config, key, configuration_section(section, user_section))
    end)
  end

  @spec read_configuration(Path.t()) :: configuration_map()
  def read_configuration(file) do
    map =
      case Toml.decode_file(file) do
        {:ok, map} -> map
        _ -> %{}
      end

    configuration(map)
  end

  @doc """
  Given a map, I decode all the needed info for a minimal node startup
  and put it in the appropriate keyword list
  """
  @spec node_settings(configuration_map()) :: Node.min_engine_configuration()
  def node_settings(configuration) do
    node = configuration["node"]
    path = node["snapshot_path"] |> String.to_atom()

    [
      {:name, node["name"] |> String.to_atom()},
      {:snapshot_path, [path | 0]},
      {:storage_data,
       %Anoma.Node.Storage{
         qualified: node["qualified"] |> String.to_atom(),
         order: node["order"] |> String.to_atom()
       }},
      {:block_storage, node["block_storage"] |> String.to_atom()},
      {:logger_table, node["logger_table"] |> String.to_atom()},
      {:ping_time, node["ping_time"] |> maybe_ping()},
      {:configuration, configuration}
    ]
  end

  @spec save(configuration_map()) :: Path.t()
  @spec save(configuration_map(), Path.t()) :: Path.t()
  def save(config, file \\ default_configuration_location()) do
    File.write!(file, serialize_config(config))
    file
  end

  @spec locate_dump_file(configuration_map()) :: Path.t() | nil
  def locate_dump_file(path) do
    dump_path = path["dump"]["dump"]

    if dump_path && File.exists?(dump_path) do
      dump_path
    else
      nil
    end
  end

  @spec launch_min(configuration_map()) :: GenServer.on_start()
  @spec launch_min(configuration_map(), [launch_option()]) ::
          GenServer.on_start()
  @doc """
  Given a parsed map with minimal node startup info I launch the node with
  the appropriate name

  ### Options
  see `t:launch_option/0` for the full list of optional arguments
  """
  def launch_min(parsed_map, options \\ []) do
    keys =
      Keyword.validate!(options,
        supervisor: nil,
        use_rocksdb: false,
        testing: false
      )

    node_settings = parsed_map |> node_settings()
    settings = Anoma.Node.start_min(node_settings)

    full_node_settings =
      [
        name: Keyword.get(node_settings, :name),
        use_rocks: keys[:use_rocksdb],
        settings: {:new_storage, settings},
        testing: keys[:testing]
      ]

    case keys[:supervisor] do
      nil ->
        Anoma.Node.start_link(full_node_settings)

      sup_settings ->
        [{Anoma.Node, full_node_settings}]
        |> Supervisor.start_link([{:strategy, :one_for_one} | sup_settings])
    end
  end

  ############################################################
  #                 Configuration Setup                      #
  ############################################################

  @spec configuration_section(section_format(), section_map()) ::
          section_map()
  defp configuration_section(section, user_map) do
    Enum.reduce(section, %{}, fn {key, predicate, default_value},
                                 configuration_section ->
      val = Map.get(user_map, key)

      Map.put(
        configuration_section,
        key,
        # we should give a warning if the predicate fails and it's not
        # nil
        if(predicate.(val), do: val, else: default_value)
      )
    end)
  end

  ############################################################
  #                Configuration Helper                      #
  ############################################################

  @spec serialize_config(configuration_map) :: String.t()
  defp serialize_config(config) do
    config
    |> Enum.to_list()
    |> Enum.map(fn {section, fields} ->
      ["[", section, "]", serialize_section(fields), "\n"]
    end)
    |> IO.chardata_to_string()
  end

  @spec serialize_section(section_map()) :: [[String.t()]]
  defp serialize_section(section) do
    section
    |> Enum.to_list()
    |> Enum.map(&serialize_pair/1)
  end

  defp serialize_pair({key, val}) do
    strings =
      if is_binary(val) do
        ["'", val, "'"]
      else
        [to_string(val)]
      end

    ["\n", key, " = " | strings]
  end

  ############################################################
  #                         Helpers                          #
  ############################################################

  defp maybe_ping(ping) do
    if is_integer(ping) do
      ping
    else
      ping |> String.to_atom()
    end
  end

  @spec is_pinger(any()) :: boolean()
  def is_pinger(field) do
    is_integer(field) || field == "no_timer"
  end
end
