defmodule Rl.Wildcard do
  @moduledoc false
  def match(wildcard, path) do
    Process.put(__MODULE__, './' ++ path)
    results = :filelib.wildcard(wildcard, __MODULE__)
    Process.delete(__MODULE__)

    results !== []
  end

  def list_dir(dir) do
    path = Process.get(__MODULE__)

    case path -- dir do
      [] ->
        {:error, :enoent}

      match ->
        handle_match(match)
    end
  end

  defp handle_match([?/ | path]) do
    handle_match(path)
  end

  defp handle_match(path) do
    part =
      path
      |> :filename.split()
      |> hd()

    {:ok, [part]}
  end

  @link_info :file.read_link_info(__ENV__.file)

  def read_link_info(file) do
    case Process.get(__MODULE__) do
      ^file ->
        {:ok, @link_info}

      _ ->
        {:error, :enoent}
    end
  end
end
