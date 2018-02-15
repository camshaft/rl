defmodule Rl.MixProject do
  use Mix.Project

  def project do
    [
      app: :rl,
      description: "Live programming environment for Elixir/Erlang",
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Rl, []},
      extra_applications: [:logger],
      env: [
        "{apps,lib,config}/**/*.{erl,ex,exs,eex,xrl,yrl,hrl}": "mix compile",
        "apps/**/mix.exs": "mix do deps.get, deps.compile, compile",
        "mix.exs": "mix do deps.get, deps.compile, compile",
        "_build/**/*.beam": [module: Rl.Watcher.Beam],
        "{apps,lib,config,test}/**/*.{erl,ex,exs,eex,xrl,yrl,hrl}": "mix test --stale"
      ]
    ]
  end

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :docs},
      {:file_system, ">= 0.0.0"}
    ]
  end
end
