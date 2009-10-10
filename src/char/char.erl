-module(char).

-include("include/records.hrl").

-export([start/0,
         init/1,
         install/0,
         uninstall/0,
         stop/0]).

start() ->
    char_config:load(main),

    Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

    {ok, Servers} = application:get_env(?MODULE, servers),

    lists:foreach(fun({_Name, Conf, _ZoneConf}) ->
                      {node, {Host, Name}} = proplists:lookup(node, Conf),
                      {aliter, Aliter} = proplists:lookup(aliter, Conf),

                      Args = "-pa " ++ Aliter ++ "/ebin",
                      {ok, Node} = slave:start_link(Host, Name, Args),

                      supervisor:start_child(?MODULE,
                                             [Node, char_srv, start_link, []])
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

stop() ->
    log:info("Stopping Char server."),
    ok.
