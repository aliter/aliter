-module(char_config).

-export([load/1]).

load(main) ->
    {ok, Cwd} = file:get_cwd(),
    load(main, Cwd ++ "/config/"),
    load(main, misc:home()).
load(main, Path) ->
    {ok, Main} = file:consult(Path ++ "main.erl"),

    {servers, {LoginConf, Servers}} = proplists:lookup(servers, Main),
    {node, {LoginHost, LoginName}} = proplists:lookup(node, LoginConf),

    lists:foreach(fun({_Name, CharConf, ZoneConf}) ->
                      {node, {CharHost, CharName}} = proplists:lookup(node, CharConf),
                      CharNode = list_to_atom(lists:concat([CharName, '@', CharHost])),

                      if
                          CharNode == node() ->
                              application:set_env(char, zone_conf, ZoneConf);
                          true ->
                              ok
                      end
                  end,
                  Servers),

    application:set_env(char, login_node, list_to_atom(lists:concat([LoginName, '@', LoginHost]))),
    application:set_env(char, servers, Servers).
