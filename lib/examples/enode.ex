defmodule Examples.ENode do
  @moduledoc """
  I give examples of the ENode.

  Currently I make networks in `Phase_1` of the example. Meaning that
  network configurations I provide, are not cloned. The given storage
  names will be written over for any shared set of storage.

  When `phase_2` of the changes come in, the signature and examples of
  this module in particular will likely change quite a bit to return
  persistent networks that other examples can use
  non-destructively. And thus every example can simply add on to
  another. However since we don't have this examples, are expected to
  clean out the state before running.
  """
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Anoma.Node
  alias Anoma.Node.{Router, Ordering, Storage}

  @doc """
  We give an example of the full node!

  We register the name and pun it with the block name. This is done
  for simplicity.
  """
  @spec fresh_full_node(Storage.t(), atom()) :: Node.t()
  def fresh_full_node(storage, name) do
    {:ok, nodes} =
      Anoma.Node.start_link_or_find_instance(
        name: name,
        testing: true,
        use_rocks: false,
        settings:
          {:new_storage,
           [
             snapshot_path: base_snapshot_path(),
             storage_data: storage,
             block_storage: name,
             ping_time: :no_timer
           ]
           |> Node.start_min()}
      )

    Node.state(nodes)
  end

  @doc """
  Simple node with the minimal dependencies for Storage
  """
  @spec simple_ordering(Storage.t()) :: Node.t()
  def simple_ordering(storage) do
    node = %Node{} = simple_storage(storage)

    assert {:ok, ordering} =
             Router.start_engine(node.router, Ordering, storage: node.storage)

    %Node{node | ordering: ordering}
  end

  @doc """
  Minimal node, with storage
  """
  @spec simple_storage(Storage.t()) :: Node.t()
  def simple_storage(storage) do
    node = simple_router()
    assert {:ok, storage} = Router.start_engine(node.router, Storage, storage)
    %Node{node | storage: storage}
  end

  @doc """
  I am just the clock engine with router support. My time starts at 0
  """
  def zero_clock() do
    node = simple_router()

    assert {:ok, clock} =
             Router.start_engine(node.router, Anoma.Node.Clock, start: 0)

    %Node{node | clock: clock}
  end

  @doc """
  I am simply a node with just a router and transport
  """
  @spec simple_router() :: Node.t()
  def simple_router() do
    assert {:ok, router, transport} = Router.start()
    %Node{router: router, transport: transport}
  end

  @spec base_snapshot_path() :: Noun.t()
  def base_snapshot_path() do
    # My special snapshot isn't popular anymore
    [999_888_777_666 | 0]
  end
end
