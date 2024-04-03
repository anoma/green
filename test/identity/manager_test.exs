defmodule AnomaTest.Identity.Manager do
  use ExUnit.Case, async: true

  alias Anoma.Crypto.Symmetric
  alias Anoma.Crypto.Id
  alias Anoma.Identity.Backend.MemoryNew
  alias Anoma.Identity.Manager
  alias Anoma.Node.Identity.Commitment
  alias Anoma.Storage

  doctest(Anoma.Identity.Manager)

  setup_all do
    storage = %Storage{
      qualified: AnomaTest.Identity.Name.Qualified,
      order: AnomaTest.Identity.Name.Order
    }

    Storage.ensure_new(storage)

    key = Symmetric.random_xchacha()

    mem = %MemoryNew{symmetric: key, storage: storage}
    [key: key, mem: mem]
  end

  test "Random key cannot be connected", %{mem: mem} do
    pair = Id.new_keypair()

    assert {:error, _} =
             Manager.connect_new(pair.external, mem, :commit_and_decrypt)
  end

  test "Generating works", %{mem: mem} do
    assert {%{commitment: com, decryption: _}, _} =
             Manager.generate(mem, :ed25519, :commit_and_decrypt)

    assert {:ok, _} = Commitment.commit(com, 555)
  end

  test "Can connect to generated id", %{mem: mem} do
    {_, pub} = Manager.generate(mem, :ed25519, :commit_and_decrypt)

    assert {:ok, %{commitment: com, decryption: _}} =
             Manager.connect_new(pub, mem, :commit_and_decrypt)

    assert {:ok, _} = Commitment.commit(com, 555)
  end

  test "Proper permissions", %{mem: mem} do
    generate = fn perms -> Manager.generate(mem, :ed25519, perms) end

    assert {%{commitment: _, decryption: _}, _} =
             generate.(:commit_and_decrypt)

    {com, _} = generate.(:commit)
    {dec, _} = generate.(:decrypt)

    refute Map.has_key?(com, :decryption) || Map.has_key?(dec, :commitment)
    assert Map.has_key?(com, :commitment) && Map.has_key?(dec, :decryption)
  end

  # test "Deleting works", %{mem: mem} do
  #   {_, pub} = Manager.generate(mem, :ed25519, :commit_and_decrypt)
  #   Manager.delete_new(pub, mem)
  #   assert {:error, _} = Manager.connect_new(pub, mem, :commit_and_decrypt)
  # end
end
