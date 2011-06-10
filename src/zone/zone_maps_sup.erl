-module(zone_maps_sup).

-behaviour(supervisor).

-include("include/records.hrl").

-export([start_link/2, server_for/1]).

-export([init/1]).


start_link(Port, Maps) ->
  log:info("Starting maps supervisor.", [{port, Port}]),

  supervisor:start_link({local, server_for(Port)}, ?MODULE, Maps).


init(Maps) ->
  Specs =
    lists:map(
      fun(M) ->
        { zone_map:server_for(M),
          {zone_map, start_link, [M]},
          permanent,
          5000,
          worker,
          [zone_map]
        }
      end,

      Maps
    ),

  {ok, {{one_for_one, 2, 60}, Specs}}.


server_for(Port) ->
  list_to_atom(lists:concat(["zone_maps_", Port, "_sup"])).
