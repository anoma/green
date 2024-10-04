defmodule ClientTest do
  use ExUnit.Case

  alias Client.Examples.EClient
  doctest Client

  test "client tests" do
    EClient.list_intents()
    EClient.add_intent()
    EClient.list_nullifiers()
    EClient.list_unrevealed_commits()
    EClient.list_unspent_resources()
    EClient.prove_something()
  end
end
