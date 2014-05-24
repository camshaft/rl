-module(rl).

-export([start/0]).
-export([compiler/2]).
-export([runner/1]).
-export([make/0]).

start() ->
  ok = application:ensure_started(rl).

compiler(Pattern, Action) ->
  rl_server:compiler(Pattern, Action).

runner(Action) ->
  rl_server:runner(Action).

make() ->
  start(),
  rl_server:compiler("src/*.?rl", fun() ->
    io:format("~n*** make app~n"),
    io:format("~s~n", [os:cmd("make app")])
  end),
  rl_server:compiler("deps/**/src/*.?rl", fun() ->
    io:format("~n*** make deps~n"),
    io:format("~s~n", [os:cmd("make deps")])
  end).
