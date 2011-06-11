-module(char).

-behaviour(supervisor).

-include("include/records.hrl").

-export([
    start_link/1,
    init/1,
    install/0,
    uninstall/0,
    stop/0]).


start_link(Config) ->
  Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

  lists:foreach(
    fun({Node, Conf}) ->
      {host, {Host, Name}} = config:find(server.host, Conf),
      {aliter, Aliter} = config:find(server.aliter, Conf),
      {ok, Node} = slave:start_link(Host, Name, aliter:path(Aliter)),

      supervisor:start_child(?MODULE, [Node, char_srv, start_link, [Conf]])
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
          1000,
          worker,
          [char_srv]
        }
      ]
    }
  }.


install() -> ok.


uninstall() -> ok.


stop() ->
  log:info("Stopping Char server."),
  ok.

