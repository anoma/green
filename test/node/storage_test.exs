defmodule AnomaTest.Node.Storage do
  use ExUnit.Case

  alias Anoma.Node.Storage

  doctest(Anoma.Node.Storage)

  setup_all do
    # base storage testing default
    Storage.start_link()

    pid =
      case Storage.start_link() do
        {:ok, pid} -> pid
        {:error, {:already_started, pid}} -> pid
      end

    [process: pid]
  end

  test "Empty Storage is absent", %{process: _process} do
    testing_atom = :QWERTZ_abc
    assert Storage.get(testing_atom) == :absent
  end

  test "Putting works", %{process: _process} do
    testing_atom = :QWERTZ_putting
    assert {:atomic, :ok} = Storage.put(testing_atom, 1)
    assert {:ok, 1} = Storage.get(testing_atom)
  end

  test "block_reads work", %{process: _process} do
    testing_atom = :QWERTZ_blocking
    assert {:atomic, :ok} = Storage.put(testing_atom, 1)
    assert {:ok, block} = Storage.blocking_read([1, testing_atom | 0])
    assert {:ok, current} = Storage.get(testing_atom)
    assert current == block
  end

  test "block_reads can read the past", %{process: _process} do
    testing_atom = :QWERTZ_blocking_past
    assert {:atomic, :ok} = Storage.put(testing_atom, 1)
    assert {:atomic, :ok} = Storage.put(testing_atom, 2)
    assert {:ok, block} = Storage.blocking_read([1, testing_atom | 0])
    assert {:ok, block_2} = Storage.blocking_read([2, testing_atom | 0])
    assert {:ok, current} = Storage.get(testing_atom)
    assert current == block_2
    assert block + 1 == block_2
  end

  test "blocking_reads must accept proper position indicators", %{process: _p} do
    assert Storage.blocking_read(:Centuri_Republic) == :error
  end

  test "blocking_reads really do block", %{process: _p} do
    testing_atom = System.unique_integer()
    home = self()

    pid =
      spawn(fn ->
        assert {:ok, value} = Storage.blocking_read([1, testing_atom | 0])
        send(home, {:read, value})
      end)

    assert Process.alive?(pid) == true
    Storage.put(testing_atom, 1)
    assert_receive {:read, 1}, 100
    assert Process.alive?(pid) == false
  end
end
