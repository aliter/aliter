-module(char_config).

-export([load/1]).

load(main) ->
    {ok, Cwd} = file:get_cwd(),
    load(main, Cwd ++ "/config/"),
    load(main, misc:home()).
load(main, Path) ->
    {ok, Main} = file:consult(Path ++ "main.erl"),

    {servers, {LoginConf, Servers}} = proplists:lookup(servers, Main),

    case lists:filter(fun({_Name, Conf, _ZoneConf}) ->
                          {node, {Host, Name}} = proplists:lookup(node, Conf),
                          Node = list_to_atom(lists:concat([Name, '@', Host])),
                          Node == node()
                      end,
                      Servers) of
        [] ->
            ok;
        [{_Name, Conf, ZoneConf}] ->
            application:set_env(char, name, proplists:get_value(name, Conf)),
            application:set_env(char, port, proplists:get_value(port, Conf)),
            application:set_env(char, zone_conf, ZoneConf)
    end,

    {node, {LoginHost, LoginName}} = proplists:lookup(node, LoginConf),
    application:set_env(char,
                        login_node,
                        list_to_atom(lists:concat([LoginName,
                                                   '@',
                                                   LoginHost]))),
    application:set_env(char, servers, Servers).
