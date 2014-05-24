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

Usage
-----

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
