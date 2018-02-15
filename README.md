# rl

Live programming environment for Elixir/Erlang.

## Installation

Add `:rl` to your dependencies

```elixir
def deps do
  [
    {:rl, "~> 0.1.0"}
  ]
end
```

You can also install it system-wide:

```elixir
mix archive.install hex rl
```

## Usage

Boot up an `IEx` shell with the following command:

```sh
$ iex -S mix rl
```

You can also run another command by appending it to the arguments.

```sh
$ iex -S mix rl phx.server
```
