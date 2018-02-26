defmodule Rl.MixProject do
  use Mix.Project

  def project do
    [
      app: :rl,
      description: "Live programming environment for Elixir/Erlang",
      version: "0.1.2",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Rl, []},
      extra_applications: [:logger],
      env: [
        "{apps,lib,config}/**/*.{ex,exs}": [module: Rl.Watcher.Formatter],
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
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:file_system, ">= 0.0.0"}
    ]
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README*"],
      maintainers: ["Cameron Bytheway"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/camshaft/rl"}
    ]
  end
end
