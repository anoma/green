defmodule Example do
  defmacro __using__(_opts) do
    quote do
      import ExUnit.Assertions
      import unquote(__MODULE__)
      Module.register_attribute(__MODULE__, :depends, accumulate: true)
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
    end
  end
end
