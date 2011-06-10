-module(login).

-include("include/records.hrl").

-export([
    start_link/1,
    install/0,
    uninstall/0,
    stop/0]).


start_link(Conf) ->
  login_srv:start_link(Conf).


install() -> ok.


uninstall() -> ok.


stop() ->
  log:info("Stopping Login server."),
  ok.

