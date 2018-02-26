defmodule Rl.Watcher.Shell do
  @moduledoc false
  def init([command]) do
    command
  end

  def handle_events(_, command) do
    run(command)

    {:ok, command}
  end

  def run(command) do
    Mix.Shell.cmd(command, [], &handle_output(command, &1))
  end

  defp handle_output("mix " <> _, "\n== " <> _ = error) do
    :io.put_chars(IO.ANSI.format([:red, :bright, error]))
  end

  defp handle_output("mix " <> _, "** (" <> _ = error) do
    :io.put_chars(IO.ANSI.format([:red, :bright, error]))
  end

  defp handle_output(command, out) do
    # IO.inspect({command, out})
    _ = command
    :io.put_chars(out)
  end
end
