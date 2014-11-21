-module(rl).

-export([start/0]).
-export([compiler/2]).
-export([runner/1]).
-export([error_handler/1]).
-export([make/0]).
-export([cmd/1]).
-export([reload/0]).

start() ->
  ok = application:ensure_started(rl).

compiler(Pattern, Action) ->
  rl_server:compiler(Pattern, Action).

runner(Action) ->
  rl_server:runner(Action).

error_handler(Action) ->
  rl_server:error_handler(Action).

make() ->
  cmd(["src/*.?rl", "make app"]).

cmd([RE, CMD]) when is_atom(RE) ->
  cmd([atom_to_list(RE), atom_to_list(CMD)]);
cmd([RE, CMD]) ->
  start(),
  rl_server:compiler(RE, fun() ->
    io:format("~n*** ~p~n", [CMD]),
    shell(CMD)
  end).

reload() ->
  rl_server:reload().

shell(Command) ->
  Port = erlang:open_port({spawn, Command}, [exit_status, binary, stderr_to_stdout]),
  rec_shell(Port, []).

rec_shell(Port, Acc) ->
  receive
    {Port, {data, Data}} ->
      io:put_chars(Data),
      rec_shell(Port, [Data|Acc]);
    {Port, {exit_status, 0}} ->
      {ok, lists:reverse(Acc)};
    {Port, {exit_status, _S}} ->
      exit(iolist_to_binary(lists:reverse(Acc)))
  end.
