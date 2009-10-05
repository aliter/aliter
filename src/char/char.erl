-module(char).
-behaviour(application).

-include("include/records.hrl").

-export([start/2,
         init/1,
         install/0,
         uninstall/0,
         shutdown/0,
         stop/1]).

start(_Type, _Args) ->
    char_config:load(main),

    Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

    {ok, Servers} = application:get_env(?MODULE, servers),
    lists:foreach(fun
                      ({Name, Char, Zone}) ->
                          {node, {CharHost, CharName}} = proplists:lookup(node, Char),
                          {aliter, CharAliter} = proplists:lookup(aliter, Char),

                          {ok, CharNode} = slave:start_link(CharHost,
                                                            CharName,
                                                            "-pa " ++ CharAliter ++ "/ebin"),

                          supervisor:start_child(?MODULE, [CharNode,
                                                           char_srv,
                                                           start_link,
                                                           [Name, Char, Zone]])
                  end,
                  Servers),

    Supervisor.

init([]) ->
    {ok, {{simple_one_for_one, 2, 60},
          [{undefined,
            {rpc, block_call, []},
            permanent,
            1000,
            worker,
            []}]}}.

install() ->
    application:set_env(mnesia,
                        dir,
                        misc:home() ++ "database/char/" ++ atom_to_list(node())),

    ok = mnesia:create_schema([node()]),

    ok = mnesia:start(),

    mnesia:create_table(char,
                        [{attributes, record_info(fields, char)},
                         {disc_copies, [node()]}]),

    mnesia:create_table(ids,
                        [{attributes, record_info(fields, ids)},
                         {disc_copies, [node()]}]),

    mnesia:dirty_update_counter(ids, char, 150000),

    mnesia:stop().

uninstall() ->
    application:set_env(mnesia, dir, misc:home() ++ "database/char/" ++ atom_to_list(node())),

    mnesia:delete_schema([node()]).

shutdown() ->
    log:info("Stopping char server."),
    application:stop(char).

stop(_State) ->
    ok.
