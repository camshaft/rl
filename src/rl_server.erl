-module(rl_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([start_link/1]).
-export([stop/0]).
-export([compiler/2]).
-export([runner/1]).
-export([error_handler/1]).
-export([reload/0]).

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
  error_handlers = [] :: [action()],
  prev :: integer()
}).

-include_lib("kernel/include/file.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  start_link(automatic).

-spec start_link(automatic | enabled) -> {ok, pid()}.
start_link(Mode) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Mode], []).

-spec stop() -> stopped.
stop() ->
  gen_server:call(?MODULE, stop).

-spec compiler(string(), action()) -> ok.
compiler(Pattern, Action) ->
  gen_server:call(?MODULE, {compiler, Pattern, Action}).

-spec runner(action()) -> ok.
runner(Action) ->
  gen_server:call(?MODULE, {runner, Action}).

-spec error_handler(action()) -> ok.
error_handler(Action) ->
  gen_server:call(?MODULE, {error_handler, Action}).

reload() ->
  gen_server:cast(?MODULE, reload).

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([automatic]) ->
  Now = erlang:localtime(),
  TRef = erlang:send_after(1000, self(), reload),
  {ok, #state{tref = TRef, prev = Now}};
init([manual]) ->
  Now = erlang:localtime(),
  {ok, #state{prev = Now}}.

-spec handle_call(any(), _, State) -> {reply, ignored, State} | {stop, normal, stopped, State} when State::#state{}.
handle_call(stop, _From, State = #state{tref = TRef}) ->
  {ok, cancel} = erlang:cancel_timer(TRef),
  {stop, normal, stopped, State};
handle_call({compiler, Pattern, Action}, _From, State = #state{compilers = Compilers}) ->
  {reply, ok, State#state{compilers = [{Pattern, Action}|Compilers]}};
handle_call({runner, Action}, _From, State = #state{runners = Runners}) ->
  {reply, ok, State#state{runners = [Action|Runners]}};
handle_call({error_handler, Action}, _From, State = #state{error_handlers = Handlers}) ->
  {reply, ok, State#state{error_handlers = [Action|Handlers]}};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast(reload, State) ->
  self() ! reload,
  {noreply, State};
handle_cast({error, {Module, Reason, Stacktrace}}, State = #state{error_handlers = Handlers}) ->
  [action(Module, Action, {Reason, Stacktrace}, false) || Action <- Handlers],
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(reload, State = #state{prev = Prev, tref = PrevTRef}) ->
  Now = erlang:localtime(),
  compile(Now, Prev, State#state.compilers),
  reload(Now, Prev, State#state.runners),
  case PrevTRef of
    undefined ->
      {noreply, State#state{prev = Now}};
    _ ->
      TRef = erlang:send_after(1000, self(), reload),
      {noreply, State#state{tref = TRef, prev = Now}}
  end;
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
      case catch execute_rl_fun(Module) of
        {'EXIT', Reason} ->
          gen_server:cast(?MODULE, {error, {Module, Reason, erlang:get_stacktrace()}});
        _ ->
          noop
      end,
      io:format(" ok.~n"),
      [action(Module, Action, reload) || Action <- Runners];
    {error, Reason} ->
      gen_server:cast(?MODULE, {error, {Module, Reason, erlang:get_stacktrace()}})
  end.

execute_rl_fun(Module) ->
  case lists:keyfind(rl, 1, Module:module_info(attributes)) of
    false ->
      noop;
    {rl, Funs} ->
      [case F of
         Fun when is_atom(Fun) ->
           Module:Fun();
         {Fun,0} when is_atom(Fun) ->
           Module:Fun()
       end || F <- Funs]
  end.

diff(Now, Prev, File) ->
  case file:read_file_info(File) of
    {ok, #file_info{mtime = Mtime}} when Mtime >= Prev, Mtime < Now ->
      modified;
    {ok, _} ->
      noop;
    {error, enoent} ->
      noop;
    {error, Reason} ->
      gen_server:cast(?MODULE, {error, {File, Reason, erlang:get_stacktrace()}}),
      noop
  end.

action(File, Action, Event) ->
  action(File, Action, Event, true).

action(File, Action, Event, Report) ->
  case catch do_action(File, Action, Event) of
    {'EXIT', Error} when Report ->
      gen_server:cast(?MODULE, {error, {File, Error, erlang:get_stacktrace()}});
    _ ->
      noop
  end.

do_action(_, _, noop) ->
  ok;
do_action(File, Action, Event) when is_pid(Action) ->
  Action ! {Event, File};
do_action(File, Action, Event) when is_function(Action) ->
  case erlang:fun_info(Action, arity) of
    {arity,0} ->
      Action();
    {arity,1} ->
      Action(File);
    {arity,2} ->
      Action(Event, File)
  end;
do_action(File, Action, Event) when is_atom(Action) ->
  Action:Event(File).
