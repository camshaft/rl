-module(rl_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

-spec init([]) -> {ok, {{supervisor:strategy(), 10, 10}, [supervisor:child_spec()]}}.
init([]) ->
  Procs = [{rl_server, {rl_server, start_link, []},
          permanent, 5000, worker, [rl_server]}],
  {ok, {{one_for_one, 10, 10}, Procs}}.
