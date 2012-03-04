-module(zone_srv_sup).

-behaviour(supervisor).

-include("include/records.hrl").

-export([start_link/2, server_for/1]).

-export([init/1]).


start_link(Port, Maps) ->
  log:info("Starting zone server supervisor.", [{port, Port}]),

  supervisor:start_link({local, server_for(Port)}, ?MODULE, {Port, Maps}).


init({Port, Maps}) ->
  MapPairs = [{M#map.name, M} || M <- Maps],

  { ok,
    { {one_for_one, 0, 60},
      [ { zone_maps_sup:server_for(Port),
          {zone_maps_sup, start_link, [Port, Maps]},
          permanent,
          infinity,
          supervisor,
          [zone_maps_sup]
        },

        { zone_srv:server_for(Port),
          {zone_srv, start_link, [Port, MapPairs]},
          permanent,
          1000,
          worker,
          [zone_srv]
        }
      ]
    }
  }.


server_for(Port) ->
  list_to_atom(lists:concat(["zone_server_", Port, "_sup"])).
