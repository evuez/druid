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
    |> Stream.map(&File.read!/1)
    |> Stream.map(&Code.string_to_quoted/1)
    |> Enum.to_list()
    |> IO.inspect()

    :ok
  end
end
