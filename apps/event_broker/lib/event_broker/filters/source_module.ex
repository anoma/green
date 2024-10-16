defmodule EventBroker.Filters.SourceModule do
  @moduledoc """
  I filter an event based on its source module.
  """

  use TypedStruct
  use EventBroker.Filter

  typedstruct enforce: true do
    @typedoc """
    I store the module representing the source of a message.

    ### Field

    - `:module` - A module name.
    """
    field(:module, module())
  end

  @spec filter(EventBroker.Event.t(), t()) :: bool()
  def filter(msg, filter_params) do
    msg.source_module == filter_params.module
  end
end
