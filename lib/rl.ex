defmodule Rl do
  @moduledoc """
  Live programming environment for Elixir/Erlang
  """

  @doc false
  def start(_, _) do
    case Mix.env() do
      :dev ->
        Supervisor.start_link(
          [
            %{
              id: Rl.Watcher,
              start: {Rl.Watcher, :start_link, []}
            }
          ],
          strategy: :one_for_one
        )

      _ ->
        {:ok, self()}
    end
  end

  @doc """
  Manually start the rl server
  """
  def start() do
    Application.ensure_all_started(:rl)
  end
end
