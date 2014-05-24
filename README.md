rl
==

automatically reload/compile modules in erlang

Installation
------------

### erlang.mk

```make
DEPS = rl
dep_rl = https://github.com/camshaft/rl.git master
```

### $ERL_LIBS (globally)

```sh
$ cd $ERL_LIBS
$ git clone https://github.com/camshaft/rl.git
$ cd rl; make
```

Usage
-----

The `rl` application will need to be started.

```erlang
application:start(rl).
```

This can be passed on the `erl` command:

```sh
$ erl -s rl
```

The default `make` settings can be loaded as well:

```sh
$ erl -s rl make
```

API
---

### rl:compiler(Wildcard, Action) -> ok when Wildcard::string(), Action::fun().

Adds a compiler to the rl server. The `Action` will be called anytime a file changed that matches the `Wildcard`:

```erlang
rl:compiler("src/*.erl", fun(File) ->
  %% do something with File
  ok
end).
```

The name of the event can also be passed. The supported events are `modified`:

```erlang
rl:compiler("src/*.erl", fun(Event, File) ->
  %% do something with File
  ok
end).
```

### rl:runner(Action) -> ok when Action::fun().

Adds a runner to the server to be called any time a module is reloaded.

```erlang
rl:runner(fun(Module) ->
  %% do something with Module
  ok
end).
```

### rl:make() -> ok.

Runs erlang.mk tasks automatically.
