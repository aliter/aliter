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

  {ok, Node} = slave:start_link(Host, Name, "-pa " ++ Aliter ++ "/ebin"),
  rpc:block_call(Node, login_srv, start_link, [Conf]).


install() ->
  application:set_env(mnesia, dir, config:db()),

  ok = mnesia:create_schema([node()]),

  ok = mnesia:start(),

  mnesia:create_table(account,
    [ {attributes, record_info(fields, account)},
      {disc_copies, [node()]}]),

  mnesia:create_table(ids,
    [ {attributes, record_info(fields, ids)},
      {disc_copies, [node()]}]),

  mnesia:dirty_update_counter(ids, account, 2000000),

  mnesia:stop().


uninstall() ->
  application:set_env(mnesia, dir, config:db()),

  mnesia:delete_schema([node()]).


stop() ->
  ok.

