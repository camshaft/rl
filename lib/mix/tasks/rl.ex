defmodule Mix.Tasks.Rl do
  use Mix.Task

  @moduledoc """
  Start the rl server through mix

  Boot up an `IEx` shell with the following command:

  ```sh
  $ iex -S mix rl
  ```

  You can also run another command by appending it to the arguments.

  ```sh
  $ iex -S mix rl phx.server
  ```
  """

  @preferred_cli_env :dev

  @spec run(OptionParser.argv()) :: :ok
  def run(args) do
    {switches, args} = Enum.split_while(args, &match?("--" <> _, &1))

    {switches, _} =
      switches
      |> OptionParser.parse!(switches: [start: :boolean, verbose: :boolean])

    Application.put_env(:rl, :verbose, switches[:verbose] === true, persistent: true)

    case {switches[:start], args} do
      {nil, []} ->
        :ok = Mix.Task.run("app.start", [])

      {nil, [_ | _]} ->
        :ok = Mix.Task.run("do", args)

      {start, []} when start === true or start === nil ->
        :ok = Mix.Task.run("app.start", [])

      _ ->
        nil
    end

    {:ok, _} = Application.ensure_all_started(:rl)
    :ok
  end
end
