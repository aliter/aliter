-module(login).
-behaviour(application).

-include("include/records.hrl").

-export([start/2,
         install/0,
         uninstall/0,
         shutdown/0,
         stop/1]).

start(_Type, _Args) ->
    login_config:load(main),

    {ok, Port} = application:get_env(?MODULE, port),

    login_srv:start_link(Port).

install() ->
    application:set_env(mnesia, dir, misc:home() ++ "database/login"),

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
    application:set_env(mnesia, dir, misc:home() ++ "database/login"),

    mnesia:delete_schema([node()]).

shutdown() ->
    log:info("Stopping login server."),
    application:stop(login).

stop(_State) ->
    ok.
