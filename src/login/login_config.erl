-module(login_config).

-export([load/1]).

load(main) ->
    {ok, Cwd} = file:get_cwd(),
    load(main, Cwd ++ "/config/"),
    load(main, aliter:home()).
load(main, Path) ->
    {ok, Main} = file:consult(Path ++ "main.erl"),

    {servers, {LoginConf, Servers}} = proplists:lookup(servers, Main),

    {node, {LoginHost, LoginName}} = proplists:lookup(node, LoginConf),
    application:set_env(login, node, list_to_atom(lists:concat([LoginName,
                                                                '@',
                                                                LoginHost]))),

    application:set_env(login, host, proplists:get_value(node, LoginConf)),
    application:set_env(login, port, proplists:get_value(port, LoginConf)),
    application:set_env(login, aliter, proplists:get_value(aliter, LoginConf)),
    application:set_env(login, servers, Servers).

