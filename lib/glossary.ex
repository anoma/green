defmodule Glossary do
  @moduledoc """
  I reference `Anoma.Glossary."transaction candidate"/0` blocks.
  I reference [anoma](`Anoma.Glossary.transaction\ candidate/0`) blocks.

  I am the Glossary module. I provide a way to define and document terms throughout the codebase.

  I am useful to link back to the definition of a term, and to provide a consistent definition
  across the codebase in docstrings.

  You can link back to Glossary items using the documentation syntax for links.

  For example, to link back to the definition of `anoma`, defined in the module `Anoma.Glossary`,
  you type `[anoma](`Anoma.Glossary.anoma/0`)` in your docstring. This will render as a link
  as the word "anoma" and link to the definition of `anoma` in the `Anoma.Glossary` module.
  """

  @doc """
  I am the __using__ macro for the Glossary module.
  I import the Glossary module and make the macros available.
  """
  defmacro __using__(_options) do
    quote do
      import unquote(__MODULE__)
    end
  end

  @doc """
  I am the `define/2` macro. I define a new entry in the glossary.

  ## Example

  ```
  define "transaction_candidate" do
    ~S(
    A transaction candidate is `t:Noun.t/0` that evaluates to a valid or
    invalid `transaction` for a specified
    `t:Anoma.Node.Executor.Worker.backend/0`
    )
  end
  ```

  I expand these definitions into functions that return the description of the definition.

  ## Example

  ```
  @transaction_candidate ~S(
  A transaction candidate is `t:Noun.t/0` that evaluates to a valid or
  invalid `transaction` for a specified
  `t:Anoma.Node.Executor.Worker.backend/0`
  )
  @spec transaction_candidate() :: String.t()
  def transaction_candidate(), do: @transaction_candidate
  ```
  """
  @spec define(String.t(), do: String.t()) :: Macro.t()
  defmacro define(label, do: description) do
    func_name = String.to_atom(label)

    quote do
      @unquote(func_name)(unquote(description))
      @doc unquote(description)
      @spec unquote(func_name)() :: String.t()
      def unquote(func_name)(), do: unquote(description)
    end
  end

  # @transaction_candidate """
  # A transaction candidate is `t:Noun.t/0` that evaluates to a valid or
  # invalid `transaction` for a specified
  # `t:Anoma.Node.Executor.Worker.backend/0`
  # """
  # @doc @transaction_candidate
  # @spec transaction_candidate() :: String.t()
  # def transaction_candidate(), do: @transaction_candidate
end
