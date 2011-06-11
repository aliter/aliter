-module(login).

-behaviour(supervisor).

-include("include/records.hrl").

-export([
    start_link/1,
    install/0,
    uninstall/0,
    stop/0]).

-export([init/1]).


start_link(Conf) ->
  {host, {Host, Name}} = config:find(server.host, Conf),
  {aliter, Aliter} = config:find(server.aliter, Conf),
  {ok, Node} = slave:start_link(Host, Name, aliter:path(Aliter)),
  supervisor:start_link({local, ?MODULE}, ?MODULE, {Node, Conf}).

init({Node, Conf}) ->
  { ok,
    { {one_for_one, 2, 60},
      [ { login,
          {rpc, block_call, [Node, login_srv, start_link, [Conf]]},
          permanent,
          1000,
          worker,
          [login_srv]
        }
      ]
    }
  }.


install() -> ok.


uninstall() -> ok.


stop() ->
  log:info("Stopping Login server."),
  ok.

