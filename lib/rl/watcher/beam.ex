defmodule Rl.Watcher.Beam do
  @moduledoc false
  def init(_) do
    nil
  end

  def handle_event(event, path, state) do
    module = path |> Path.basename(".beam") |> String.to_atom()
    :code.purge(module)
    :code.delete(module)

    if event !== :deleted do
      Code.ensure_loaded(module)
    end

    {:ok, state}
  end
end
