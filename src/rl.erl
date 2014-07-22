-module(rl).

-export([start/0]).
-export([compiler/2]).
-export([runner/1]).
-export([make/0]).
-export([cmd/1]).
-export([reload/0]).

start() ->
  ok = application:ensure_started(rl).

compiler(Pattern, Action) ->
  rl_server:compiler(Pattern, Action).

runner(Action) ->
  rl_server:runner(Action).

make() ->
  cmd(["src/*.?rl", "make app"]).

cmd([RE, CMD]) when is_atom(RE) ->
  cmd([atom_to_list(RE), atom_to_list(CMD)]);
cmd([RE, CMD]) ->
  start(),
  rl_server:compiler(RE, fun() ->
    io:format("~n*** ~p~n", [CMD]),
    io:format("~s~n", [os:cmd(CMD)])
  end).

reload() ->
  rl_server:reload().
