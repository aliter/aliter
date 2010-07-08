-module(config).

-export([home/0,
         home/1,
         db/0,
         scripts/0,
         scripts/1,
         load/0,
         load_path/1,
         find/2,
         set_env/2,
         get_env/2]).

home() ->
    home("").
home(Path) ->
    {ok, [[Home]]} = init:get_argument(home),

    lists:concat([Home, "/.aliter/", Path]).

db() ->
    home(lists:concat(["db/", node(), "/"])).

scripts() ->
    scripts("").
scripts(Script) ->
    lists:concat([home("scripts/"), Script]).

load() ->
    filelib:ensure_dir(home("db/")),
    filelib:ensure_dir(home("scripts/")),

    LoadLogin = load_path("config/login"),

    {host, {LoginHost, LoginName}} = find(server.host, LoadLogin),
    LoginNode = list_to_atom(lists:concat([LoginName, "@", LoginHost])),

    Login = [{node, LoginNode} | LoadLogin],

    {ok, Char} = file:list_dir(home("config/char")),
    Chars = lists:map(fun(Node) ->
                          {list_to_atom(Node), load_path("config/char/" ++ Node)}
                      end,
                      Char),

    {ok, Zone} = file:list_dir(home("config/zone")),
    Zones = lists:map(fun(Node) ->
                          {list_to_atom(Node), load_path("config/zone/" ++ Node)}
                      end,
                      Zone),

    {ok, API} = file:consult(home("config/api.erl")),

    CharsFinal = lists:map(fun({Node, Conf}) ->
                               {zone, ZoneNode} = find(server.zone, Conf),
                               {ZoneNode, ZoneConf} = proplists:lookup(ZoneNode, Zones),
                               {Node, Conf ++ [{login, Login}, {zone, ZoneConf}]}
                           end, Chars),

    ZonesFinal = lists:map(fun({Node, Conf}) ->
                               {char, CharNode} = find(server.char, Conf),
                               {CharNode, CharConf} = proplists:lookup(CharNode, Chars),
                               {Node, Conf ++ [{login, Login}, {char, CharConf}]}
                           end, Zones),

    LoginFinal = Login ++ [{chars, CharsFinal}, {zones, ZonesFinal}],

    {LoginFinal, CharsFinal, ZonesFinal, API}.

load_path(Base) ->
    {ok, Files} = file:list_dir(home(Base)),

    lists:map(fun(Filename) ->
                  {ok, Config} = file:consult(home(Base ++ "/" ++ Filename)),
                  {list_to_atom(filename:basename(Filename, ".erl")), Config}
              end,
              lists:filter(fun(Filename) ->
                               filename:extension(Filename) == ".erl"
                           end,
                           Files)).

get_env(App, Setting) ->
    find(Setting, application:get_all_env(App)).

find(By, Acc) when By == []; Acc == undefined ->
    Acc;
find([Lookup | Rest], {_Key, Acc}) ->
    find(Rest, proplists:lookup(Lookup, Acc));
find(Setting, From) ->
    Tokens = lists:map(fun list_to_atom/1,
                       string:tokens(atom_to_list(Setting), ".")),

    find(tl(Tokens), proplists:lookup(hd(Tokens), From)).

set_env(App, Config) ->
    lists:foreach(fun({Key, Val}) ->
                      application:set_env(App, Key, Val)
                  end, Config).
