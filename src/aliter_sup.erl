-module(aliter_sup).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1]).


start_link(StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).


init([]) ->
  {Login, Char, Zone, API} = config:load(),

  { ok, {{one_for_one, 2, 60},
    [ {login, {login, start_link, [Login]}, permanent, 1000, worker, []},
      {char, {char, start_link, [Char]}, permanent, 1000, worker, []},
      {zone, {zone, start_link, [Zone]}, permanent, 1000, worker, []},
      {api, {api, start_link, [API]}, permanent, 1000, worker, []}]}}.

