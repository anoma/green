defmodule AnomaTest.Node.Dumper do
  use TestHelper.TestMacro, async: true

  alias Examples.ENode.EDumper

  test "examples" do
    EDumper.dumped_node()
  end
end
