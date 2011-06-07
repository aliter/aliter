-module(char).

-include("include/records.hrl").

-export([
    start_link/1,
    init/1,
    install/0,
    uninstall/0,
    stop/0]).


start_link(Config) ->
  Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

  lists:foreach(fun({Node, Conf}) ->
    {host, {Host, Name}} = config:find(server.host, Conf),
    {aliter, Aliter} = config:find(server.aliter, Conf),

    Args = "-pa " ++ Aliter ++ "/ebin",
    {ok, Node} = slave:start_link(Host, Name, Args),

    supervisor:start_child(?MODULE, [Node, char_srv, start_link, [Conf]])
  end,
  Config),

  Supervisor.


init([]) ->
  { ok,
    { {simple_one_for_one, 2, 60},
      [ { undefined,
          {rpc, block_call, []},
          permanent,
          1000,
          workeriiii,
          []
        }
      ]
    }
  }.


install() ->
  application:set_env(mnesia, dir, config:db()),

  ok = mnesia:create_schema([node()]),

  ok = mnesia:start(),

  mnesia:create_table(char,
    [{attributes, record_info(fields, char)}, {disc_copies, [node()]}]),

  mnesia:create_table(ids,
    [{attributes, record_info(fields, ids)}, {disc_copies, [node()]}]),

  mnesia:dirty_update_counter(ids, char, 150000),

  mnesia:stop().


uninstall() ->
  application:set_env(mnesia, dir, config:db()),

  mnesia:delete_schema([node()]).


stop() ->
  log:info("Stopping Char server."),
  ok.

