defmodule Anoma.ShieldedResource.ShieldedTransaction do
  @moduledoc """
  I am a shielded resource machine transaction.
  """

  @behaviour Noun.Nounable.Kind

  require Logger

  alias __MODULE__
  use TypedStruct
  alias Anoma.ShieldedResource.ProofRecord
  alias Anoma.ShieldedResource.PartialTransaction
  alias Anoma.ShieldedResource.ComplianceOutput
  alias Anoma.Node.DummyStorage, as: Storage

  typedstruct enforce: true do
    # TODO: The roots, commitments, and nullifiers can be eliminated. We can
    # obtain them from public inputs. Then we can make the same improvement for
    # transparent transactions. However, they are used in the executor atm.
    field(:roots, list(binary()), default: [])
    field(:commitments, list(binary()), default: [])
    field(:nullifiers, list(binary()), default: [])
    field(:partial_transactions, list(PartialTransaction.t()), default: [])

    # When the tx is not finalized, the delta is the collection of private keys
    # When the tx is finalized, the delta is the binding signature
    field(:delta, binary(), default: <<>>)
  end

  @spec from_noun(Noun.t()) :: {:ok, t()}
  def from_noun([
        roots,
        commitments,
        nullifiers,
        partial_transactions
        | delta
      ]) do
    ptxs =
      partial_transactions
      |> Noun.list_nock_to_erlang()
      |> Enum.map(&PartialTransaction.from_noun/1)

    checked = Enum.all?(ptxs, &(elem(&1, 0) == :ok))

    if checked do
      {:ok,
       %ShieldedTransaction{
         roots: roots |> Noun.list_nock_to_erlang(),
         commitments: commitments |> Noun.list_nock_to_erlang(),
         nullifiers: nullifiers |> Noun.list_nock_to_erlang(),
         partial_transactions: Enum.map(ptxs, &elem(&1, 1)),
         delta: delta
       }}
    else
      :error
    end
  end

  defimpl Noun.Nounable, for: __MODULE__ do
    def to_noun(transaction = %ShieldedTransaction{}) do
      {
        transaction.roots,
        transaction.commitments,
        transaction.nullifiers,
        transaction.partial_transactions,
        transaction.delta
      }
      |> Noun.Nounable.to_noun()
    end
  end

  defimpl Anoma.RM.Transaction, for: __MODULE__ do
    def commitments(%ShieldedTransaction{commitments: cm}), do: cm
    def nullifiers(%ShieldedTransaction{nullifiers: nf}), do: nf

    def storage_commitments(tx), do: commitments(tx)
    def storage_nullifiers(tx), do: nullifiers(tx)

    def compose(tx1, tx2) do
      unless Anoma.RM.Trans.compose_pre_check(tx1, tx2) do
        nil
      else
        %ShieldedTransaction{
          roots: tx1.roots ++ tx2.roots,
          commitments: tx1.commitments ++ tx2.commitments,
          nullifiers: tx1.nullifiers ++ tx2.nullifiers,
          partial_transactions:
            tx1.partial_transactions ++ tx2.partial_transactions,
          delta: tx1.delta ++ tx2.delta
        }
      end
    end

    # TODO: We can return roots, commitments, and nullifiers instead of just a
    # boolean value so that we can get rid of them in the ShieldedTransaction struct. We
    # can apply the same improvement to the transparent Transaction.
    def verify(transaction = %ShieldedTransaction{}) do
      # check proofs
      all_proofs_valid =
        for ptx <- transaction.partial_transactions,
            reduce: true do
          acc ->
            result = PartialTransaction.verify(ptx)
            Logger.debug("partial_transactions result: #{inspect(result)}")
            acc && result
        end

      # Decode the compliance_output
      compliance_outputs =
        transaction.partial_transactions
        |> Enum.flat_map(fn ptx ->
          ptx.compliance_proofs
          |> Enum.map(fn proof_record ->
            ComplianceOutput.from_public_input(proof_record.public_inputs)
          end)
        end)

      # Collect binding public keys
      binding_pub_keys = get_binding_pub_keys(compliance_outputs)

      # Collect binding signature msgs
      binding_messages = ShieldedTransaction.get_binding_messages(transaction)

      delta_valid =
        Cairo.sig_verify(
          binding_pub_keys,
          binding_messages,
          transaction.delta |> :binary.bin_to_list()
        )

      # Collect resource logics from compliance proofs
      resource_logics_from_compliance =
        compliance_outputs
        |> Enum.reduce([], &[&1.output_logic | [&1.input_logic | &2]])
        |> Enum.reverse()

      # Compute the program hash of resource logic proofs
      resource_logic_from_program =
        transaction.partial_transactions
        |> Enum.flat_map(fn ptx ->
          ptx.logic_proofs
          |> Enum.map(fn proof_record ->
            proof_record.public_inputs
            |> :binary.bin_to_list()
            |> Cairo.get_program_hash()
            |> :binary.list_to_bin()
          end)
        end)

      resource_logic_valid =
        resource_logics_from_compliance == resource_logic_from_program

      all_proofs_valid && delta_valid && resource_logic_valid
    end

    def cm_tree(_tx, _storage) do
      CommitmentTree.new(
        CommitmentTree.Spec.cairo_poseidon_cm_tree_spec(),
        nil
      )
    end

    def resource_existence_check(transaction, storage) do
      for root <- transaction.roots, reduce: true do
        acc ->
          root_key = ["rm", "commitment_root", root]
          acc && Storage.get(storage, root_key) == {:ok, true}
      end
    end

    @spec get_binding_pub_keys(list(binary())) :: list(byte())
    defp get_binding_pub_keys(compliance_outputs) do
      compliance_outputs
      |> Enum.reduce(
        [],
        &[:binary.bin_to_list(&1.delta_x <> &1.delta_y) | &2]
      )
    end
  end

  def get_compliance_outputs(transaction) do
    transaction.partial_transactions
    |> Enum.flat_map(fn ptx ->
      ptx.compliance_proofs
      |> Enum.map(fn proof_record ->
        proof_record.public_inputs
        |> ComplianceOutput.from_public_input()
      end)
    end)
  end

  @spec get_binding_messages(ShieldedTransaction.t()) :: list(list(byte()))
  def get_binding_messages(tx = %ShieldedTransaction{}) do
    (tx.nullifiers ++ tx.commitments)
    |> Enum.map(&:binary.bin_to_list/1)
  end

  # sign(binding signature) the transation when the transaction is balanced and finalized
  @spec sign(ShieldedTransaction.t()) :: ShieldedTransaction.t()
  defp sign(tx = %ShieldedTransaction{}) do
    msgs = get_binding_messages(tx)

    binding_signature =
      tx.delta
      |> :binary.bin_to_list()
      |> Cairo.sign(msgs)
      |> :binary.list_to_bin()

    %ShieldedTransaction{tx | delta: binding_signature}
  end

  # complete and sign the tx
  @spec finalize(ShieldedTransaction.t()) :: ShieldedTransaction.t()
  def finalize(tx = %ShieldedTransaction{}) do
    compliance_outputs = get_compliance_outputs(tx)
    roots = Enum.map(compliance_outputs, & &1.root)
    commitments = Enum.map(compliance_outputs, & &1.output_cm)
    nullifiers = Enum.map(compliance_outputs, & &1.nullifier)

    %ShieldedTransaction{
      tx
      | roots: roots,
        commitments: commitments,
        nullifiers: nullifiers
    }
    |> sign()
  end

  @spec generate_resource_logic_proofs(list(binary())) ::
          {:ok, list(ProofRecord.t())} | :error
  defp generate_resource_logic_proofs(jsons) do
    resource_logics_unchecked =
      Enum.map(jsons, & &1["logic"])
      |> Enum.map(&File.read/1)

    resource_logic_inputs_unchecked =
      Enum.map(jsons, & &1["logic_input"])
      |> Enum.map(&File.read/1)

    checked_1 = Enum.all?(resource_logics_unchecked, &(elem(&1, 0) == :ok))

    checked_2 =
      Enum.all?(resource_logic_inputs_unchecked, &(elem(&1, 0) == :ok))

    with true <- checked_1 && checked_2 do
      resource_logics =
        Enum.map(resource_logics_unchecked, &elem(&1, 1))

      resource_logic_inputs =
        Enum.map(resource_logic_inputs_unchecked, &elem(&1, 1))

      proofs_unchecked =
        Enum.zip_with(
          resource_logics,
          resource_logic_inputs,
          &ProofRecord.generate_proof/2
        )

      checked = Enum.all?(proofs_unchecked, &(elem(&1, 0) == :ok))

      with true <- checked do
        {:ok, Enum.map(proofs_unchecked, &elem(&1, 1))}
      else
        false -> :error
      end
    else
      false -> :error
    end
  end

  @spec hash_resource_logic(ProofRecord.t()) :: binary()
  defp hash_resource_logic(pr) do
    pr.public_inputs
    |> :binary.bin_to_list()
    |> Cairo.get_program_hash()
    |> :binary.list_to_bin()
  end

  @spec update_resource_jsons(list(binary()), list(binary())) ::
          list(binary())
  defp update_resource_jsons(jsons, logic_hashes) do
    hex_logic_hashes =
      Enum.map(logic_hashes, &binary_to_hex/1)

    Enum.zip_with(
      jsons,
      hex_logic_hashes,
      &put_in(&1["logic"], &2)
    )
    |> Enum.map(&elem(pop_in(&1["logic_input"]), 1))
  end

  @spec hex_to_32_byte_binary(binary()) :: binary()
  defp hex_to_32_byte_binary(hex_string) do
    hex_num_string_0 = String.replace_prefix(hex_string, "0x", "")

    hex_num_string =
      if rem(String.length(hex_num_string_0), 2) == 0 do
        hex_num_string_0
      else
        "0" <> hex_num_string_0
      end

    with {:ok, str} <- Base.decode16(hex_num_string) do
      binary_padding(str, 32)
    else
      _ -> <<>>
    end
  end

  @spec binary_padding(binary(), integer()) :: binary()
  defp binary_padding(binary, size) when byte_size(binary) <= size do
    <<0::size((size - byte_size(binary)) * 8)>> <> binary
  end

  @spec binary_to_hex(binary()) :: String.t()
  defp binary_to_hex(binary) do
    "0x" <> Base.encode16(binary)
  end

  @spec create_from_compliance_input_files(list(Path.t())) ::
          {:ok, ShieldedTransaction.t()} | :error
  def create_from_compliance_input_files(compliance_input_files) do
    compliance_pre_inputs_content_unchecked =
      Enum.map(compliance_input_files, &File.read/1)

    checked =
      Enum.all?(
        compliance_pre_inputs_content_unchecked,
        &(elem(&1, 0) == :ok)
      )

    with true <- checked do
      compliance_pre_inputs_unchecked =
        Enum.map(compliance_pre_inputs_content_unchecked, &elem(&1, 1))
        |> Enum.map(&Jason.decode(&1, objects: :ordered_objects))

      checked =
        Enum.all?(compliance_pre_inputs_unchecked, &(elem(&1, 0) == :ok))

      with true <- checked do
        compliance_pre_inputs =
          Enum.map(compliance_pre_inputs_unchecked, &elem(&1, 1))

        input_resources =
          Enum.map(compliance_pre_inputs, & &1["input"])

        output_resources =
          Enum.map(compliance_pre_inputs, & &1["output"])

        with {:ok, input_logic_proofs} <-
               generate_resource_logic_proofs(input_resources),
             {:ok, output_logic_proofs} <-
               generate_resource_logic_proofs(output_resources) do
          input_logic_hashes =
            Enum.map(input_logic_proofs, &hash_resource_logic/1)

          output_logic_hashes =
            Enum.map(output_logic_proofs, &hash_resource_logic/1)

          updated_input_resources =
            update_resource_jsons(input_resources, input_logic_hashes)

          updated_output_resources =
            update_resource_jsons(output_resources, output_logic_hashes)

          compliance_inputs_unchecked =
            Enum.zip_with(
              compliance_pre_inputs,
              updated_output_resources,
              &put_in(&1["output"], &2)
            )
            |> Enum.zip_with(
              updated_input_resources,
              &put_in(&1["input"], &2)
            )
            |> Enum.map(&Jason.encode/1)

          checked =
            Enum.all?(compliance_inputs_unchecked, &(elem(&1, 0) == :ok))

          with true <- checked do
            compliance_inputs =
              Enum.map(compliance_inputs_unchecked, &elem(&1, 1))

            compliance_proofs_unchecked =
              Enum.map(
                compliance_inputs,
                &ProofRecord.generate_compliance_proof/1
              )

            checked =
              Enum.all?(compliance_proofs_unchecked, &(elem(&1, 0) == :ok))

            with true <- checked do
              compliance_proofs =
                Enum.map(compliance_proofs_unchecked, &elem(&1, 1))

              ptx = %PartialTransaction{
                logic_proofs:
                  Enum.zip_with(
                    input_logic_proofs,
                    output_logic_proofs,
                    &[&1, &2]
                  )
                  |> Enum.concat(),
                compliance_proofs: compliance_proofs
              }

              priv_keys =
                Enum.map(compliance_pre_inputs, & &1["rcv"])
                |> Enum.map(&hex_to_32_byte_binary/1)
                |> Enum.reduce(&<>/2)

              {:ok,
               %ShieldedTransaction{
                 partial_transactions: [ptx],
                 delta: priv_keys
               }}
            else
              false -> :error
            end
          else
            false -> :error
          end
        else
          _ -> :error
        end
      else
        false -> :error
      end
    else
      false -> :error
    end
  end
end
