-module(zone).

-include("include/records.hrl").

-export([start_link/1, install/0, uninstall/0, stop/0]).


start_link(Config) ->
  {_Node, Conf} = proplists:lookup(node(), Config),
  zone_master_sup:start_link(Conf).


install() -> ok.


uninstall() -> ok.


stop() ->
  log:info("Stopping Zone server."),
  ok.
