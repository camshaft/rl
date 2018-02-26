defmodule Rl.Watcher.Formatter do
  @moduledoc false
  def init(events) do
    events
  end

  def handle_events(events, state) do
    Rl.Watcher.Shell.run("mix format")

    events
    |> Stream.map(&elem(&1, 0))
    |> Enum.into(MapSet.new())
    |> flush()

    {:ok, state}
  end

  defp flush(prev_events) do
    receive do
      {:file_event, _, {file, _}} = event ->
        if !MapSet.member?(prev_events, file) do
          :timer.send_after(100, event)
        end

        flush(prev_events)
    after
      10 ->
        :ok
    end
  end
end
