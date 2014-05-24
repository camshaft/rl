-module(rl_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([compiler/2]).
-export([runner/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type action() :: string() | fun() | atom().

-record(state, {
  tref :: reference(),
  compilers = [] :: [{string(), action()}],
  runners = [] :: [action()],
  prev :: integer()
}).

-include_lib("kernel/include/file.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
  gen_server:call(?MODULE, stop).

-spec compiler(string(), action()) -> ok.
compiler(Pattern, Action) ->
  gen_server:cast(?MODULE, {compiler, Pattern, Action}).

-spec runner(action()) -> ok.
runner(Action) ->
  gen_server:cast(?MODULE, {runner, Action}).

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([]) ->
  Now = erlang:localtime(),
  TRef = erlang:send_after(1000, self(), reload),
  {ok, #state{tref = TRef, prev = Now}}.

-spec handle_call(any(), _, State) -> {reply, ignored, State} | {stop, normal, stopped, State} when State::#state{}.
handle_call(stop, _From, State = #state{tref = TRef}) ->
  {ok, cancel} = erlang:cancel_timer(TRef),
  {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast({compiler, Pattern, Action}, State = #state{compilers = Compilers}) ->
  {noreply, State#state{compilers = [{Pattern, Action}|Compilers]}};
handle_cast({runner, Action}, State = #state{runners = Runners}) ->
  {noreply, State#state{runners = [Action|Runners]}};
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(reload, State = #state{prev = Prev}) ->
  Now = erlang:localtime(),
  compile(Now, Prev, State#state.compilers),
  reload(Now, Prev, State#state.runners),
  TRef = erlang:send_after(1000, self(), reload),
  {noreply, State#state{tref = TRef, prev = Now}};
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, State, _) -> {ok, State} when State::#state{}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal.

compile(_, _, []) ->
  ok;
compile(Now, Prev, [{Pattern, Action}|Compilers]) ->
  Files = filelib:wildcard(Pattern),
  compile_files(Now, Prev, Files, Action),
  compile(Now, Prev, Compilers);
compile(Now, Prev, [{Pattern, Cwd, Action}|Compilers]) ->
  Files = filelib:wildcard(Pattern, Cwd),
  compile_files(Now, Prev, Files, Action),
  compile(Now, Prev, Compilers).

compile_files(Now, Prev, Files, Action) ->
  [action(File, Action, diff(Now, Prev, File)) || File <- Files].

reload(Now, Prev, Runners) ->
  Loaded = code:all_loaded(),
  [case diff(Now, Prev, Filename) of
    modified ->
      case code:is_sticky(Module) of
        false ->
          reload_module(Module, Runners);
        _ ->
          noop
      end;
    _ ->
      noop
  end || {Module, Filename} <- Loaded, is_list(Filename)].

reload_module(Module, Runners) ->
  io:format("reloading ~p ...", [Module]),
  code:purge(Module),
  case code:load_file(Module) of
    {module, Module} ->
      io:format(" ok.~n"),
      [action(Module, Action, reload) || Action <- Runners];
    {error, Reason} ->
      io:format(" error: ~p.~n", [Reason])
  end.

diff(Now, Prev, File) ->
  case file:read_file_info(File) of
    {ok, #file_info{mtime = Mtime}} when Mtime >= Prev, Mtime < Now ->
      modified;
    {ok, _} ->
      noop;
    {error, enoent} ->
      removed;
    {error, Reason} ->
      error_logger:error_msg("Error reading ~s's file info ~p~n", [File, Reason]),
      noop
  end.

action(File, Action, Event) ->
  try do_action(File, Action, Event)
  catch
    Class:Error ->
      error_logger:error_msg("Error while compiling ~p~n~p:~p~n~p", [File, Class, Error, erlang:get_stacktrace()])
  end.

do_action(_, _, noop) ->
  ok;
do_action(File, Action, Event) when is_pid(Action) ->
  Action ! {Event, File};
do_action(File, Action, Event) when is_function(Action) ->
  Action(Event, File);
do_action(File, Action, Event) when is_atom(Action) ->
  Action:Event(File).
