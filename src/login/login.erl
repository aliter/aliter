-module(login).

-include("include/records.hrl").

-export([start/0,
         install/0,
         uninstall/0,
         stop/0]).

start() ->
    login_config:load(main),

    {ok, {Host, Name}} = application:get_env(?MODULE, host),
    {ok, Aliter} = application:get_env(?MODULE, aliter),
    {ok, Node} = slave:start_link(Host, Name, "-pa " ++ Aliter ++ "/ebin"),

    rpc:block_call(Node, login_srv, start_link, []).

install() ->
    application:set_env(mnesia, dir, aliter:home() ++ "database/login"),

    ok = mnesia:create_schema([node()]),

    ok = mnesia:start(),

    mnesia:create_table(account,
                        [{attributes, record_info(fields, account)},
                         {disc_copies, [node()]}]),

    mnesia:create_table(ids,
                        [{attributes, record_info(fields, ids)},
                         {disc_copies, [node()]}]),

    mnesia:dirty_update_counter(ids, account, 2000000),

    mnesia:stop().

uninstall() ->
    application:set_env(mnesia, dir, aliter:home() ++ "database/login"),

    mnesia:delete_schema([node()]).

stop() ->
    ok.
