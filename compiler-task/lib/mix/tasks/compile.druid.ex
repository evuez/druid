defmodule Mix.Tasks.Compile.Druid do
  use Mix.Task
  @recursive true

  @moduledoc """
  """

  @doc false
  def run(_args) do
    config = Mix.Project.config()
    srcs = config[:elixirc_paths]

    all_paths = MapSet.new(Mix.Utils.extract_files(srcs, ["ex"]))

    all_paths
    |> Stream.map(fn path -> {path, File.read!(path)} end)
    |> Stream.map(fn {path, source} -> Code.string_to_quoted!(source, file: path) end)
    |> Stream.map(fn ast -> System.cmd("druid", [inspect(ast, limit: :infinity)]) end)
    |> Enum.to_list()
    |> IO.inspect()

    :ok
  end
end
