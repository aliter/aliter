-module(login).

-include("include/records.hrl").

-export([
    start_link/1,
    install/0,
    uninstall/0,
    stop/0]).


start_link(Conf) ->
  {host, {Host, Name}} = config:find(server.host, Conf),
  {aliter, Aliter} = config:find(server.aliter, Conf),
  {ok, Node} = slave:start_link(Host, Name, aliter:path(Aliter)),
  rpc:block_call(Node, login_srv, start_link, [Conf]).


install() -> ok.


uninstall() -> ok.


stop() ->
  log:info("Stopping Login server."),
  ok.

