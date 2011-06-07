-module(zone).

-include("include/records.hrl").

-export([start_link/1, init/1, install/0, uninstall/0, stop/0]).


start_link(Config) ->
  Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

  lists:foreach(
    fun({Node, Conf}) ->
      log:info("Starting.", [{server, Node}]),

      {host, {Host, Name}} = config:find(server.host, Conf),
      {aliter, Aliter} = config:find(server.aliter, Conf),

      {ok, Node} = slave:start_link(Host, Name, aliter:path(Aliter)),

      supervisor:start_child(?MODULE, [Node, zone_master, start_link, [Conf]])
    end,

    Config
  ),

  Supervisor.


init([]) ->
  { ok,
    { {simple_one_for_one, 2, 60},
      [ { undefined,
          {rpc, block_call, []},
          permanent,
          infinity,
          supervisor,
          []
        }
      ]
    }
  }.


install() ->
  application:set_env(mnesia, dir, config:db()),

  ok = mnesia:create_schema([node()]),

  ok = mnesia:start(),

  mnesia:create_table(item,
                      [{attributes, record_info(fields, item)},
                        {disc_copies, [node()]}]),
  mnesia:create_table(monster,
                      [{attributes, record_info(fields, monster)},
                        {disc_copies, [node()]}]),
  mnesia:create_table(guild,
                      [{attributes, record_info(fields, guild)},
                        {disc_copies, [node()]}]),

  mnesia:create_table(ids,
                      [{attributes, record_info(fields, ids)},
                        {disc_copies, [node()]}]),

  mnesia:dirty_update_counter(ids, guild, 150000),

  mnesia:stop().


uninstall() ->
  application:set_env(mnesia, dir, config:db()),

  mnesia:delete_schema([node()]).


stop() ->
  log:info("Stopping Zone server."),
  ok.
