-module(zone_zones_sup).

-behaviour(supervisor).

-include("include/records.hrl").

-export([start_link/1]).

-export([init/1]).


start_link(Conf) ->
  log:debug("Starting zones supervisor."),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Conf).


init(Conf) ->
  AllMaps = maps:read_cache("priv/maps"),

  {zones, Zones} = config:find(server.zones, Conf),
  Specs = lists:map(
    fun({Port, ZoneMaps}) ->
      Names = [list_to_binary(X) || X <- ZoneMaps],

      log:debug("Starting slave.", [{port, Port}, {maps, Names}]),

      Maps =
        lists:filter(
          fun(M) ->
            lists:member(M#map.name, Names)
          end,
          AllMaps
        ),

      { zone_srv_sup:server_for(Port),
        {zone_srv_sup, start_link, [Port, Maps]},
        permanent,
        infinity,
        supervisor,
        [zone_srv_sup]
      }
    end,
    Zones
  ),

  {ok, {{one_for_one, 2, 60}, Specs}}.  
