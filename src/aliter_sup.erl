-module(aliter_sup).
-behaviour(supervisor).

-export([start_link/1,
         init/1]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init([]) ->
    {ok, {{one_for_one, 2, 60},
          [{login, {login, start, []}, permanent, 1000, worker, []},
           {char, {char, start, []}, permanent, 1000, worker, []},
           % {zone, {zone, start, []}, permanent, 1000, worker, []},
           {api, {api, start_link, []}, permanent, 1000, worker, []}]}}.
