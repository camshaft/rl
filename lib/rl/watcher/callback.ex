defmodule Rl.Watcher.Callback do
  @moduledoc false
  def init(events) do
    events
  end

  def handle_event(event, path, state) do
    case Map.fetch(state, event) do
      {:ok, fun} ->
        fun.(event, path)

      _ ->
        nil
    end

    {:ok, state}
  end
end
