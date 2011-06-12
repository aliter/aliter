-module(zone_master_sup).

-behaviour(supervisor).

-include("include/records.hrl").

-export([start_link/1]).

-export([init/1]).


start_link(Conf) ->
  log:debug("Starting master supervisor."),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Conf).


init(Conf) ->
  { ok,
    { {one_for_all, 2, 60},
      [ { zone_zones_sup,
          {zone_zones_sup, start_link, [Conf]},
          permanent,
          infinity,
          supervisor,
          [zone_zones_sup]
        },

        { zone_master,
          {zone_master, start_link, [Conf]},
          permanent,
          5000,
          worker,
          [zone_master]
        }
      ]
    }
  }.
