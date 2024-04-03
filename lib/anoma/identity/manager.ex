defmodule Anoma.Identity.Manager do
  @moduledoc """
  I am responsible for generating, connecting, and deleting identities.

  I abstracts a uniform interface over identities created with
  different "backends", including, for example:

    - internal identities stored in local memory
    - internal identities stored in a hardware device, e.g. Ledger
    - internal identities stored in a browser extension
    - internal identities stored in another machine accessible over the network

  When an identity is generated or connected, the I do not return the
  internal identity directly, but rather returns handles to the
  corresponding commitment and decryption engine instances, which can
  be used to generate commitments by and decrypt data encrypted to,
  respectively, the internal identity (which is still kept in whatever
  backend is in use).
  """

  require IEx
  require IEx.Pry
  alias Anoma.Identity.{Backend, Capabilities, Parameters}
  alias Anoma.Node.Identity.{Commitment, Decryption}
  alias Anoma.Crypto.Id
  alias Anoma.Storage

  @type resp(t) :: {:ok, t} | {:error, String.t()}

  @type instance() :: %{commitment: pid(), decryption: pid()}

  @spec generate(Backend.t(), Parameters.t(), Capabilities.t()) ::
          resp({instance(), Id.Extern.t()})
  def generate(mem = %Backend.MemoryNew{}, :ed25519, cap) do
    pair = Id.new_keypair()
    salted_pair = Id.salt_keys(pair, mem.symmetric)

    Storage.put(
      mem.storage,
      [name_space(), salted_pair.external.sign],
      salted_pair
    )

    with {:ok, links} <- start_links(pair, cap) do
      {links, pair.external}
    end
  end

  @spec connect(Id.Extern.t(), Backend.t(), Capabilities.t()) ::
          resp(instance())
  def connect(id, mem = %Backend.Memory{}, cap) do
    salted_key = Id.salt_keys(id, mem.symmetric)

    get_tx = fn -> :mnesia.read({mem.table, salted_key.sign}) end

    with {:atomic, [identity]} <- :mnesia.transaction(get_tx) do
      identity
      |> Id.decode()
      |> Id.unsalt_keys(mem.symmetric)
      |> start_links(cap)
    else
      _ ->
        {:error, "Failed to find key"}
    end
  end

  @spec connect_new(Id.Extern.t(), Backend.t(), Capabilities.t()) ::
          resp(instance())
  def connect_new(id, mem = %Backend.MemoryNew{}, cap) do
    salted_key = Id.salt_keys(id, mem.symmetric)

    with {:ok, identity} <-
           Storage.get(mem.storage, [name_space(), salted_key.sign]) do
      identity
      |> Id.unsalt_keys(mem.symmetric)
      |> start_links(cap)
    else
      _ ->
        IEx.pry()
        {:error, "Failed to find key"}
    end
  end

  @doc """
  I delete the given key.

  Note that depending on the backend the following could happen:

  1. If there is an active Decryption and Commitment engine and the
     backend is a memory backend, then the keys can still be used for
     singing and decryption. However once these engines die there is
     not a way to get them back after deletion.
  2. If there is an active Decryption and Commitment engine and the
     backend is external, then they can't be used anymore as the
     actual keys are gone from the external device.
  3. One can no longer connect to the key given it does not exist in
     the system anymore
  """
  @spec delete(Id.Extern.t(), Backend.t()) :: resp(nil)
  def delete(id, mem = %Backend.Memory{}) do
    salted_key = Id.salt_keys(id, mem.symmetric)

    # If we can read the salted public key, then you can decrypt the
    # private key as well, making it known to whoever is submitting
    trans =
      fn -> :mnesia.delete({mem.table, salted_key.sign}) end
      |> :mnesia.transaction()

    case trans do
      {:atomic, :ok} -> {:ok, nil}
      _ -> {:error, "bad transaction, failed to delete key"}
    end
  end

  @spec delete_new(Id.Extern.t(), Backend.t()) :: resp(nil)
  def delete_new(id, mem = %Backend.MemoryNew{}) do
    salted_key = Id.salt_keys(id, mem.symmetric)
    res = Storage.delete_key(mem.storage, [name_space(), salted_key.sign])

    case res do
      {:atomic, :ok} -> {:ok, nil}
      _ -> {:error, "bad transaction, failed to delete key"}
    end
  end

  ############################################################
  #                           Helpers                        #
  ############################################################

  @spec start_links(Id.t(), Capabilities.t()) :: resp(instance())
  def start_links(data, :commit) do
    with {:ok, cpid} <-
           Commitment.start_link({data.internal.sign, data.kind_sign}) do
      {:ok, %{commitment: cpid}}
    end
  end

  def start_links(data, :decrypt) do
    with {:ok, dpid} <-
           Decryption.start_link(
             {data.external.encrypt, data.external.encrypt, data.kind_encrypt}
           ) do
      {:ok, %{decryption: dpid}}
    end
  end

  def start_links(data, :commit_and_decrypt) do
    with {:ok, cpid} <-
           Commitment.start_link({data.internal.sign, data.kind_sign}),
         {:ok, dpid} <-
           Decryption.start_link(
             {data.external.encrypt, data.external.encrypt, data.kind_encrypt}
           ) do
      {:ok, %{commitment: cpid, decryption: dpid}}
    end
  end

  @base_name_space "identity_manager"
  def name_space(), do: @base_name_space
end
