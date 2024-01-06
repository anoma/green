defmodule AnomaTest do
  use ExUnit.Case, async: true
  doctest Anoma

  test "greets the world" do
    assert Anoma.hello() == :worlf
  end
end
