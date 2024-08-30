defmodule Examples.ENock do
  use Memoize

  require ExUnit.Assertions
  import ExUnit.Assertions

  alias Examples.ENode.EStorage
  alias Anoma.Transaction
  alias Anoma.Node.{Router, Ordering}
  alias Anoma.Node.Executor.Worker
  alias Examples.ENode
  alias Examples.ECrypto
  alias Noun.Format

  alias Anoma.Node
  alias Anoma.Node.Storage

  ####################################################################
  ##                        Resource Logics                         ##
  ####################################################################

  # saves 5 microseconds on second use, should we bother, is this a
  # bad pattern?
  @spec zero_delta_logic() :: Noun.t()
  defmemo zero_delta_logic() do
    Format.parse_always("[[5 [1 0] [0 446]] 0 0]")
  end

  @doc """
  The counter arm.

  Availiable through `counter:logics` core.
  """

  @spec counter_arm() :: Noun.t()
  def counter_arm() do
    """
    [ 6
      [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 118] 0 2]
      [6
        [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 238] 0 2]
        [6
          [5 [1 1] 8 [9 1.406 0 511] 9 2 10 [6 0 958] 0 2]
          [6 [5 [1 0] 0 446] [0 0] 6 [0 3.570] [1 0] 1 1] 1 1] 1 1]
          1
          1
        ]
    """
    |> Format.parse_always()
  end

  @spec counter_logic() :: Noun.t()
  def counter_logic() do
    [
      counter_arm(),
      0 | Nock.logics_core()
    ]
  end

  ####################################################################
  ##                           Noun fun                             ##
  ####################################################################

  @spec one_two() :: Noun.t()
  def one_two() do
    assert {:ok, term = [1 | 2]} = Noun.Format.parse("[1 2]"),
           "Noun should parse sensibly"

    term
  end

  @spec nesting_noun() :: Noun.t()
  def nesting_noun() do
    assert {:ok, term = [1, [3 | 5] | 2]} =
             Noun.Format.parse("[1 [[3 5] 2]]"),
           "Basic Nouns should parse sensibly"

    term
  end

  def incorrectly_nested_noun() do
    term = Noun.Format.parse("[[[[1]]]]")
    assert :error = term
    term
  end

  @spec incorrectly_ending() :: :error
  def incorrectly_ending() do
    term = Noun.Format.parse("[[[[")
    assert :error = term
    term
  end

  @spec incorrectly_starting() :: :error
  def incorrectly_starting() do
    term = Noun.Format.parse("]]]]")
    assert :error = term
    term
  end

  @spec indexed_noun() :: Noun.t()
  def indexed_noun() do
    assert {:ok, term} = Noun.Format.parse("[[4 5] [12 13] 7]")
    assert {:ok, term} == Noun.axis(1, term)
    assert {:ok, [4 | 5]} == Noun.axis(2, term)
    assert {:ok, [12 | 13]} == Noun.axis(6, term)
    assert {:ok, 7} == Noun.axis(7, term)
    assert {:ok, 4} == Noun.axis(4, term)
    assert {:ok, 5} == Noun.axis(5, term)
    assert {:ok, 12} == Noun.axis(12, term)
    assert {:ok, 13} == Noun.axis(13, term)
    term
  end

  @spec replacing_terms() :: Noun.t()
  def replacing_terms() do
    assert {:ok, term} = Noun.replace(2, 2, indexed_noun())
    assert {:ok, 2} == Noun.axis(2, term)
    term
  end

  ####################################################################
  ##                       Scrying: Phase 1                         ##
  ####################################################################

  @spec successful_inc :: {Node.t(), Nock.t()}
  def successful_inc() do
    storage = Node.raw_storage(EStorage.august_node("successful_inc"))
    {node, env} = {anode(storage), env(storage)}

    node.ordering
    |> Ordering.new_order([
      %Transaction{
        index: 1,
        id: EStorage.random_id(),
        addr: Router.self_addr()
      }
    ])

    formula = [9, 2, 10, [6, 1 | EStorage.random_id()], 0 | 1]

    assert {:ok, [EStorage.miki_key() | EStorage.lucky_value() + 1]} ==
             Nock.nock(miki_increment(), formula, env),
           "We should be scrying the right place"

    {node, env}
  end

  @spec unsuccessful_inc :: Node.t()
  def unsuccessful_inc() do
    # sets up the network nicely
    {node, env} = successful_inc()
    # no devil key ☹
    formula = [9, 2, 10, [6, 1 | EStorage.random_id()], 0 | 1]

    assert :error == Nock.nock(devil_increment(), formula, env),
           "Key ought not to be found"

    node
  end

  @spec miki_increment_kv_tx() :: Worker.transaction()
  def miki_increment_kv_tx() do
    {:kv, miki_increment_candidate()}
  end

  def miki_increment_candidate() do
    increment_counter_val(EStorage.miki_key())
  end

  def miki_increment() do
    assert {:ok, increment} =
             Nock.nock(miki_increment_candidate(), [9, 2, 0 | 1], env())

    increment
  end

  def devil_increment() do
    devil_formula = increment_counter_val(EStorage.devil_key())
    assert {:ok, increment} = Nock.nock(devil_formula, [9, 2, 0 | 1], env())
    increment
  end

  ####################################################################
  ##                          Jetted Cores                          ##
  ##    Requires special testing to ensure they behave properly.    ##
  ####################################################################

  @doc """
  The decrement arm in the tests core.

  Availiable through `use-dec:tests` core.
  """
  @spec dec_arm() :: Noun.t()
  def dec_arm() do
    "[8 [9 342 0 1.023] 9 2 10 [6 0 14] 0 2]" |> Format.parse_always()
  end

  @spec dec() :: Noun.t()
  def dec() do
    sample = 999
    core = [dec_arm(), sample | Nock.logics_core()]

    assert Nock.nock(core, [9, 2, 0 | 1]) == {:ok, 998}

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 22], 0 | 1]) == {:ok, 21}

    assert Nock.nock(core, [9, 2, 10, [6, 1 | <<22>>], 0 | 1]) == {:ok, 21},
           "dec works on internally binary atoms"

    core
  end

  @doc """
  A cue arm for taking cue:anoma out of the logics core environment.

  Can be gotten by defining gate locally as

  =localcue   =>  logics  |=  a=@  (cue a)

  and then grabbing the arm of localcue.
  """

  @spec cue_arm() :: Noun.t()
  def cue_arm() do
    "[8 [9 94 0 63] 9 2 10 [6 0 14] 0 2]" |> Format.parse_always()
  end

  @spec cue() :: Noun.t()
  def cue() do
    sample = 999
    core = [cue_arm(), sample | Nock.logics_core()]

    core
  end

  @doc """
  A cue arm for taking jam:anoma out of the logics core environment.

  Can be gotten by defining gate locally as

  =localjam   =>  logics  |=  a=@  (jam a)

  and then grabbing the arm of localjam.
  """

  @spec jam_arm() :: Noun.t()
  def jam_arm() do
    "[8 [9 22 0 63] 9 2 10 [6 0 14] 0 2]" |> Format.parse_always()
  end

  @spec jam() :: Noun.t()
  def jam() do
    sample = 999
    core = [jam_arm(), sample | Nock.logics_core()]

    core
  end

  @doc """
  The sign arm for taking sign:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localsign   =>  logics  |=  [a=@ b=@]  (sign [a b])

  and then grabbing the arm of localsign.
  """

  @spec sign_arm() :: Noun.t()
  def sign_arm() do
    "[8 [9 10 0 31] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Format.parse_always()
  end

  @spec sign() :: Noun.t()
  def sign() do
    sample = [999 | 888]
    core = [sign_arm(), sample | Nock.logics_core()]

    valid_args = [ECrypto.blood_msg() | ECrypto.londo().internal.sign]
    invalid_args = [ECrypto.blood_msg() | ECrypto.londo().external.sign]

    assert {:ok, ECrypto.blood_l_signed()} ==
             Nock.nock(core, [9, 2, 10, [6, 1 | valid_args], 0 | 1])

    assert :error ==
             Nock.nock(core, [9, 2, 10, [6, 1 | invalid_args], 0 | 1]),
           "Can't sign with one's public key!"

    core
  end

  @doc """
  The verify arm for taking verify:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localverify   =>  logics  |=  [a=@ b=@]  (verify [a b])

  and then grabbing the arm of localverify.
  """

  @spec verify_arm() :: Noun.t()
  def verify_arm() do
    "[8 [9 4 0 31] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Format.parse_always()
  end

  @spec verify() :: Noun.t()
  def verify() do
    sample = [999 | 888]
    core = [verify_arm(), sample | Nock.logics_core()]

    valid_args = [ECrypto.blood_l_signed() | ECrypto.londo().external.sign]
    invalid_args = [ECrypto.blood_msg() | ECrypto.londo().internal.sign]

    assert {:ok, [0 | ECrypto.blood_msg()]} ==
             Nock.nock(core, [9, 2, 10, [6, 1 | valid_args], 0 | 1])

    assert {:ok, 0} ==
             Nock.nock(core, [9, 2, 10, [6, 1 | invalid_args], 0 | 1]),
           "Can't verify with someone's private key"

    core
  end

  @doc """
  The sign-detatched arm for taking sign-detached:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localsigndetached   =>  logics  |=  [a=@ b=@]  (sign-detached [a b])

  and then grabbing the arm of localsighdetached.
  """

  @spec sign_detatched_arm() :: Noun.t()
  def sign_detatched_arm() do
    "[8 [9 23 0 31] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Format.parse_always()
  end

  @spec sign_detatched() :: Noun.t()
  def sign_detatched() do
    sample = [999 | 888]
    core = [sign_detatched_arm(), sample | Nock.logics_core()]

    valid_args = [ECrypto.blood_msg() | ECrypto.londo().internal.sign()]

    assert {:ok, ECrypto.blood_l_signed_detached()} ==
             Nock.nock(core, [9, 2, 10, [6, 1 | valid_args], 0 | 1])

    assert :error == Nock.nock(core, [9, 2, 10, [6, 1, <<3>> | 5], 0 | 1])
    assert :error == Nock.nock(core, [9, 2, 10, [6, 1, <<3>> | <<5>>], 0 | 1])
    assert :error == Nock.nock(core, [9, 2, 10, [6, 1, 3 | <<5>>], 0 | 1])
    core
  end

  @doc """
  The verify-detatched arm for taking verify-detached:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localverifydetached   =>  logics  |=  [a=@ b=@ c=@]  (verify-detached [a b])

  and then grabbing the arm of localverifydetached.
  """

  @spec verify_detatched_arm() :: Noun.t()
  def verify_detatched_arm() do
    "[8 [9 22 0 31] 9 2 10 [6 7 [0 3] [0 12] [0 26] 0 27] 0 2]"
    |> Format.parse_always()
  end

  @spec verify_detatched() :: Noun.t()
  def verify_detatched() do
    sample = [999 | 888]
    core = [verify_detatched_arm(), sample | Nock.logics_core()]

    sign = ECrypto.blood_l_signed_detached()
    valid = [sign, ECrypto.blood_msg() | ECrypto.londo().external.sign]
    wrong_msg = [sign, <<3>> | ECrypto.londo().external.sign]
    invalid_key = [sign, ECrypto.blood_msg() | ECrypto.londo().internal.sign]

    invalid_size = [
      <<3>>,
      ECrypto.blood_msg() | ECrypto.londo().external.sign
    ]

    all_invalid = [<<3>>, <<4>> | <<55>>]

    assert {:ok, 0} == Nock.nock(core, [9, 2, 10, [6, 1 | valid], 0 | 1])
    assert {:ok, 1} == Nock.nock(core, [9, 2, 10, [6, 1 | wrong_msg], 0 | 1])

    assert {:ok, 1} ==
             Nock.nock(core, [9, 2, 10, [6, 1 | invalid_key], 0 | 1]),
           "private key deosn't verify"

    assert {:ok, 1} ==
             Nock.nock(core, [9, 2, 10, [6, 1 | invalid_size], 0 | 1]),
           "Gracefully fail on invalidly sized messages"

    assert {:ok, 1} ==
             Nock.nock(core, [9, 2, 10, [6, 1 | all_invalid], 0 | 1]),
           "Everything being wrong, doesn't excuse a crash"

    core
  end

  @doc """
  The bex arm for taking bex:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localbex   =>  logics  |=  a=@  (bex a)

  and then grabbing the arm of localbex.
  """

  @spec bex_arm() :: Noun.t()
  def bex_arm() do
    "[8 [9 4 0 127] 9 2 10 [6 0 14] 0 2]" |> Format.parse_always()
  end

  @spec bex() :: Noun.t()
  def bex() do
    sample = 888
    core = [bex_arm(), sample | Nock.logics_core()]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 2], 0 | 1]) == {:ok, 4}
    assert Nock.nock(core, [9, 2, 10, [6, 1 | 5], 0 | 1]) == {:ok, 32}

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 28], 0 | 1]) ==
             {:ok, 268_435_456}

    core
  end

  @doc """
  The mix arm for taking mix:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localmix   =>  logics  |=  [a=@ b=@]  (mix [a b])

  and then grabbing the arm of locamix.
  """

  @spec mix_arm() :: Noun.t()
  def mix_arm() do
    "[8 [9 4 0 63] 9 2 10 [6 7 [0 3] [0 12] 0 13] 0 2]"
    |> Format.parse_always()
  end

  @spec mix() :: Noun.t()
  def mix() do
    sample = [0 | 0]
    core = [mix_arm(), sample | Nock.logics_core()]

    assert {:ok, 6} == Nock.nock(core, [9, 2, 10, [6, 1, 3 | 5], 0 | 1])
    assert {:ok, 0} == Nock.nock(core, [9, 2, 10, [6, 1, 11 | 11], 0 | 1])

    core
  end

  @doc """
  The mat arm for taking mat:anoma from the logics core environment.

  Can be gotten by defining gate locally as:

  =localmat   =>  logics  |=  a  (mat a)

  and then grabbing the arm of locamix.
  """

  @spec mat_arm() :: Noun.t()
  def mat_arm() do
    "[8 [9 43 0 63] 9 2 10 [6 0 14] 0 2]" |> Format.parse_always()
  end

  # Please make some assertions ☹
  @spec mat() :: Noun.t()
  def mat() do
    sample = 0
    core = [mat_arm(), sample | Nock.logics_core()]

    core
  end

  ############################################################
  ##                      Block Cores                       ##
  ############################################################

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =llsh   =>  logics  |=  a=@  lsh:block
  """

  @spec lsh(Noun.t()) :: Noun.t()
  def lsh(value) do
    block_calling_biop(value, 90)
  end

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =lmet   =>  logics  |=  a=@  met:block
  """

  @spec met(Noun.t()) :: Noun.t()
  def met(value) do
    block_calling_mono(value, 190)
  end

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =luend   =>  logics  |=  a=@  luend:block
  """

  @spec uend(Noun.t()) :: Noun.t()
  def uend(value) do
    block_calling_biop(value, 367)
  end

  @doc """
  I am an lash arm in the block door.

  My index inside the door can be seen by asking to dump the logic of
  =rsh   =>  logics  |=  a=@  rsh:block
  """

  @spec rsh(Noun.t()) :: Noun.t()
  def rsh(value) do
    block_calling_biop(value, 767)
  end

  @doc """
  I evaluate met at block size 0 and gate-input 28.

  met(0) evaluates the gate of the block door at block size 0,
  [6 1 28] replaces the sample with 28.
  """

  @spec met0() :: Noun.t()
  def met0() do
    met = met(0)
    assert Nock.nock(met, [9, 2, 10, [6, 1 | 28], 0 | 1]) == {:ok, 5}
    met
  end

  @doc """
  I evaluate met at block size 1 and gate-input 28.

  met(1) evaluates the gate of the block door at block size 1,
  [6 1 28] replaces the sample with 28.
  """

  @spec met1() :: Noun.t()
  def met1() do
    met = met(1)
    assert Nock.nock(met, [9, 2, 10, [6, 1 | 28], 0 | 1]) == {:ok, 3}
    met
  end

  @doc """
  I evaluate met at block size 2 and gate-input 28.

  met(2) evaluates the gate of the block door at block size 2,
  [6 1 28] replaces the sample with 28.
  """

  @spec met2() :: Noun.t()
  def met2() do
    met = met(2)
    assert Nock.nock(met, [9, 2, 10, [6, 1 | 28], 0 | 1]) == {:ok, 2}
    met
  end

  @doc """
  I evaluate uend at block size 0 and gate-input [5 80].

  uend(0) evaluates the gate of the block door at block size 0,
  [6 1 5 80] replaces the sample with [5 80].
  """

  @spec uend0() :: Noun.t()
  def uend0() do
    uend = uend(0)
    assert {:ok, 16} == Nock.nock(uend, [9, 2, 10, [6, 1, 5 | 80], 0 | 1])
    uend
  end

  @doc """
  I evaluate uend at block size 1 and gate-input [3 80] and [4 80].

  uend(1) evaluates the gate of the block door at block size 1,
  [6 1 3 80] replaces the sample with [3 80],
  [6 1 4 80] replaces the sample with [3 80]
  """

  @spec uend1() :: Noun.t()
  def uend1() do
    uend = uend(1)
    assert {:ok, 16} == Nock.nock(uend, [9, 2, 10, [6, 1, 3 | 80], 0 | 1])
    assert {:ok, 80} == Nock.nock(uend, [9, 2, 10, [6, 1, 4 | 80], 0 | 1])
    uend
  end

  @doc """
  I evaluate lsh at block size 0 and gate-input [2 6].

  lsh(0) evaluates the gate of the block door at block size 0,
  [6 1 2 6] replaces the sample with [2 6].
  """

  @spec lsh0() :: Noun.t()
  def lsh0() do
    lsh = lsh(0)
    assert {:ok, 24} == Nock.nock(lsh, [9, 2, 10, [6, 1, 2 | 6], 0 | 1])
    lsh
  end

  @doc """
  I evaluate lsh at block size 1 and gate-input [2 6].

  lsh(1) evaluates the gate of the block door at block size 1,
  [6 1 2 6] replaces the sample with [2 6].
  """

  @spec lsh1() :: Noun.t()
  def lsh1() do
    lsh = lsh(1)
    assert {:ok, 96} == Nock.nock(lsh, [9, 2, 10, [6, 1, 2 | 6], 0 | 1])
    lsh
  end

  @doc """
  I evaluate lsh at block size 1 and gate-input [2 6].

  lsh(2) evaluates the gate of the block door at block size 2,
  [6 1 2 6] replaces the sample with [2 6].
  """

  @spec lsh2() :: Noun.t()
  def lsh2() do
    lsh = lsh(2)
    assert {:ok, 1536} == Nock.nock(lsh, [9, 2, 10, [6, 1, 2 | 6], 0 | 1])
    lsh
  end

  @doc """
  I evaluate rsh at block size 0 and gate-input [2 40].

  rsh(0) evaluates the gate of the block door at block size 0,
  [6 1 2 40] replaces the sample with [2 40].
  """

  @spec rsh0() :: Noun.t()
  def rsh0() do
    rsh = rsh(0)
    assert {:ok, 10} == Nock.nock(rsh, [9, 2, 10, [6, 1, 2 | 40], 0 | 1])
    rsh
  end

  @doc """
  I evaluate rsh at block size 1 and gate-input [2 40].

  rsh(1) evaluates the gate of the block door at block size 1,
  [6 1 2 40] replaces the sample with [2 40].
  """

  @spec rsh1() :: Noun.t()
  def rsh1() do
    rsh = rsh(1)
    assert {:ok, 2} == Nock.nock(rsh, [9, 2, 10, [6, 1, 2 | 40], 0 | 1])
    rsh
  end

  @doc """
  I evaluate rsh at block size 2 and gate-input [2 40].

  rsh(2) evaluates the gate of the block door at block size 2,
  [6 1 1 40] replaces the sample with [1 40].
  """

  @spec rsh2() :: Noun.t()
  def rsh2() do
    rsh = rsh(2)
    assert {:ok, 2} == Nock.nock(rsh, [9, 2, 10, [6, 1, 1 | 40], 0 | 1])
    rsh
  end

  ####################################################################
  ##                          Normal Cores                          ##
  ####################################################################

  @doc """
  I am the battery of the fib:tests gate of the anoma stadard library.

  You can dump me by calling

  .*  fib:tests  [0 2]
  """

  @spec factorial_arm() :: Noun.t()
  def factorial_arm() do
    "
    [ 8
      [1 1 0]
      8
      [ 1
        6
        [5 [0 30] 1 0]
        [0 13]
        9
        2
        10
        [30 8 [9 342 0 4.095] 9 2 10 [6 0 62] 0 2]
        10
        [6 [8 [9 20 0 4.095] 9 2 10 [6 [0 29] 0 28] 0 2] 0 12]
        0
        1
      ]
      9
      2
      0
      1
    ]" |> Format.parse_always()
  end

  @spec factorial() :: Noun.t()
  def factorial() do
    sample = 1
    core = [factorial_arm(), sample | Nock.logics_core()]

    assert Nock.nock(core, [9, 2, 10, [6, 1 | 7], 0 | 1]) == {:ok, 13},
           "calling into the standard library works well"

    core
  end

  ####################################################################
  ##                             Helpers                            ##
  ####################################################################

  @spec block_calling_biop(Noun.t(), Noun.t()) :: Noun.t()
  defp block_calling_biop(value, index) do
    # get the battery for calling a 2-argument gate located at index
    # `index` at a block door evaluated with block size value `value`

    # check the index of the block by defining block locally
    # =lblock   =>  logics  |=  a=@  block
    # then check the gate index by dumping
    # =lgateblock   =>  logics  |=  a=@  gate:block
    # finally check how the door inputs its block-size by evaluating
    # =>  logics  !=(~(gate block val))
    # with different values

    arm =
      Noun.Format.parse_always(
        "[8 [8 [9 10 0 127] 9 #{index} 10 [6 7 [0 3] 1 #{value}] 0 2] 9 2 10 [6 [0 28] 0 29] 0 2]"
      )

    sample = [999 | 888]
    [arm, sample | Nock.logics_core()]
  end

  @spec block_calling_mono(Noun.t(), Noun.t()) :: Noun.t()
  defp block_calling_mono(value, index) do
    # get the battery for calling a 2-argument gate located at index
    # `index` at a block door evaluated with block size value `value`

    # check the index of the block by defining block locally
    # =lblock   =>  logics  |=  a=@  block
    # then check the gate index by dumping
    # =lgateblock   =>  logics  |=  a=@  gate:block
    # finally check how the door inputs its block-size by evaluating
    # =>  logics  !=(~(gate block val))
    # with different values

    arm =
      Noun.Format.parse_always(
        "[8 [8 [9 10 0 127] 9 #{index} 10 [6 7 [0 3] 1 #{value}] 0 2] 9 2 10 [6 0 14] 0 2]"
      )

    sample = 999
    [arm, sample | Nock.logics_core()]
  end

  @spec increment_counter_val(Noun.t()) :: Noun.t()
  def increment_counter_val(val) do
    arm = [[1 | val], 4, 12, [1 | 0], [0 | 6], 1, val | 0]
    sample = 0
    [[8, [1 | sample], [1 | arm], 0 | 1] | Nock.logics_core()]
  end

  # [%ctr 0]
  @spec zero_counter(Noun.t()) :: Noun.t()
  def zero_counter(val) do
    arm = [1, val | 0]
    sample = 0
    [[8, [1 | sample], [1 | arm], 0 | 1] | Nock.logics_core()]
  end

  ####################################################################
  ##                    Cueing to a Jam Session                     ##
  ####################################################################

  # I want the results to be useful here... sadly they are not ☹
  # Maybe move the tests to the core itself... idk

  def jamming_and_cueing() do
    jam_and_cue(0, 2)
    jam_and_cue(1, 12)
    jam_and_cue(2, 72)
    jam_and_cue(19, 2480)
    jam_and_cue(581_949_002, 1_191_831_557_952)
    jam_and_cue([0 | 19], 39689)
    jam_and_cue([1 | 1], 817)
    jam_and_cue([10_000 | 10_000], 4_952_983_169)
    jam_and_cue([65536 | <<0, 0, 1>>], 158_376_919_809)
    jam_and_cue([999_999_999 | 999_999_999], 1_301_217_674_263_809)
    jam_and_cue([222, 444 | 888], 250_038_217_192_960_129)
    jam_and_cue([[107 | 110] | [107 | 110]], 635_080_761_093)

    jam_and_cue(
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9 | 10],
      25_681_224_503_728_653_597_984_370_231_065
    )

    jam_and_cue(
      [99, 100, 101, 102, 103, 104 | 0],
      223_372_995_869_285_333_705_242_560_449
    )

    jam_and_cue(
      [[222, 444 | 888] | [222, 444 | 888]],
      170_479_614_045_978_345_989
    )

    jam_and_cue(
      [[0 | 1], [1 | 2], [2 | 3], [3 | 4] | 0],
      11_976_248_475_217_237_797
    )

    jam_and_cue(
      [
        [0 | 1],
        [1 | 2],
        [2 | 3],
        [3 | 4],
        [4 | 5],
        [5 | 6],
        [6 | 7],
        [7 | 8],
        [8 | 9] | 0
      ],
      7_694_087_033_387_855_647_747_387_855_514_468_399_947_749_137_782_565
    )

    jam_and_cue(
      [
        [0 | 1],
        [2 | 3],
        [4 | 5],
        [6 | 7],
        [8 | 9],
        [10 | 11],
        [12 | 13],
        [14 | 15],
        [16 | 17],
        [18 | 19],
        [20 | 21] | 0
      ],
      308_947_677_754_874_070_959_300_747_182_056_036_528_545_493_781_368_831_595_479_491_505_523_344_414_501
    )

    assert dec() ==
             dec() |> Nock.Jam.jam() |> Nock.Cue.cue!()

    :ok
  end

  def jam_and_cue(jam_value, cue_value) do
    assert Noun.equal(jam_value, Nock.Cue.cue!(cue_value))
    assert cue_value == Nock.Jam.jam(Noun.normalize_noun(jam_value))
  end

  ####################################################################
  ##                             Phase 1                            ##
  ####################################################################

  @spec anode() :: Node.t()
  @spec anode(Storage.t()) :: Node.t()
  defmemo anode(storage \\ raw_storage()) do
    ENode.simple_ordering(storage)
  end

  @spec env() :: Nock.t()
  @spec env(Storage.t()) :: Nock.t()
  def env(storage \\ raw_storage()) do
    %Nock{
      snapshot_path: ENode.base_snapshot_path(),
      ordering: anode(storage).ordering
    }
  end

  @spec raw_storage() :: Storage.t()
  def raw_storage() do
    %Storage{
      qualified: __MODULE__.Qualified,
      order: __MODULE__.Order
    }
  end
end
