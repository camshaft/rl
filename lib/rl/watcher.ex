defmodule Rl.Watcher do
  @moduledoc false
  require Logger
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, [])
  end

  @events [
    :created,
    :updated,
    :deleted
  ]

  def init([]) do
    {:ok, watcher_pid} = FileSystem.start_link(dirs: [System.cwd!()])
    FileSystem.subscribe(watcher_pid)
    state = load_config()

    state =
      Map.merge(state, %{
        watcher_pid: watcher_pid,
        buffer: [],
        timeout: nil
      })

    {:ok, state}
  end

  defmacrop catch_error(expr, fallback) do
    quote do
      try do
        {:ok, value} = unquote(expr)
        value
      rescue
        error ->
          Logger.error(Exception.format(:error, error, System.stacktrace()))
          unquote(fallback)
      catch
        kind, error ->
          Logger.error(Exception.format(kind, error, System.stacktrace()))
          unquote(fallback)
      end
    end
  end

  def handle_info(
        {:file_event, watcher_pid, event},
        %{watcher_pid: watcher_pid, buffer: buffer, timeout: timeout} = state
      ) do
    case handle_file_event(event) do
      nil ->
        {:noreply, state}

      {path, event} ->
        buffer = [{path, event} | buffer]

        if timeout do
          :timer.cancel(timeout)
        end

        timeout = :timer.send_after(500, :flush)

        {:noreply, %{state | buffer: buffer, timeout: timeout}}
    end
  end

  def handle_info(:flush, %{buffer: []} = state) do
    {:noreply, %{state | buffer: [], timeout: nil}}
  end

  def handle_info(:flush, %{watchers: watchers, buffer: buffer} = state) do
    filtered =
      buffer
      |> Enum.reduce(%{}, fn {path, event}, acc ->
        case Map.fetch(acc, path) do
          {:ok, ^event} ->
            acc

          {:ok, :deleted} ->
            acc

          {:ok, :created} ->
            acc

          {:ok, :updated} when event === :created ->
            %{acc | path => event}

          _ ->
            Map.put(acc, path, event)
        end
      end)

    paths =
      for {path, _} = item <- filtered do
        char_path =
          path
          |> Path.relative_to_cwd()
          |> to_charlist()

        {char_path, item}
      end

    watchers =
      for {wildcard, module, bulk?, state} <- watchers do
        state =
          paths
          |> Stream.filter(fn {path, _} ->
            Rl.Wildcard.match(wildcard, path)
          end)
          |> Enum.map(fn {_, {path, event}} ->
            {path, event}
          end)
          |> case do
            [] ->
              state

            events ->
              if bulk? do
                catch_error(module.handle_events(events, state), state)
              else
                Enum.reduce(events, state, fn {path, event}, state ->
                  catch_error(module.handle_event(event, path, state), state)
                end)
              end
          end

        {wildcard, module, bulk?, state}
      end

    {:noreply, %{state | watchers: watchers, buffer: [], timeout: nil}}
  end

  def handle_info(
        {:file_event, watcher_pid, :stop},
        %{watcher_pid: watcher_pid, watchers: watchers} = state
      ) do
    # TODO shutdown watchers
    _ = watchers
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  defp handle_file_event({path, events}) do
    case {:created in events || :attribute in events, :deleted in events, :modified in events} do
      {true, true, _} ->
        # temp file
        nil

      {true, _, _} ->
        {path, :created}

      {_, true, _} ->
        {path, :deleted}

      {_, _, true} ->
        {path, :updated}

      _ ->
        nil
    end
  end

  defp load_config(prev \\ %{}) do
    config = Application.get_all_env(:rl)
    {verbose, config} = Keyword.pop(config, :verbose)
    config = Keyword.drop(config, [:included_applications])
    _ = prev
    # TODO shut down any previous watchers that are no longer present
    %{
      verbose: verbose,
      watchers:
        config
        |> Stream.map(fn {path, opts} ->
          wildcard = to_charlist(path)

          ## TODO only initialize new workers
          {module, state} = setup_watcher(opts)

          {wildcard, module, function_exported?(module, :handle_events, 2), state}
        end)
        |> Enum.filter(fn watcher ->
          pattern = elem(watcher, 0)

          try do
            Rl.Wildcard.match(pattern, 'test.ex')
            true
          catch
            _, _ ->
              Logger.error("Invalid wildcard pattern: #{inspect(pattern)}")
              false
          end
        end)
    }
  end

  defp setup_watcher(command) when is_binary(command) do
    setup_watcher(%{module: Rl.Watcher.Shell, opts: [command]})
  end

  defp setup_watcher(module) when is_atom(module) do
    setup_watcher(%{module: module, opts: []})
  end

  defp setup_watcher(args) when is_list(args) do
    setup_watcher(:maps.from_list(args))
  end

  defp setup_watcher(%{module: module} = config) do
    {module, module.init(Map.get(config, :opts, []))}
  end

  defp setup_watcher(events) do
    opts =
      events
      |> Stream.filter(fn
        {event, _fun} when event not in @events ->
          Logger.warn("Invalid event #{inspect(event)}")
          false

        {_event, fun} when not is_function(fun, 2) ->
          Logger.warn("Invalid event callback #{inspect(fun)}")
          false

        _ ->
          true
      end)
      |> Enum.into(%{})

    setup_watcher(%{module: Rl.Watcher.Callback, opts: opts})
  end
end
