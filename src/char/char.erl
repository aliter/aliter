-module(char).

-include("include/records.hrl").

-export([
    start_link/1,
    install/0,
    uninstall/0,
    stop/0]).


start_link(Config) ->
  {_Node, Conf} = proplists:lookup(node(), Config),
  char_srv:start_link(Conf).


install() -> ok.


uninstall() -> ok.


stop() ->
  log:info("Stopping Char server."),
  ok.

