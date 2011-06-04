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
         get_env/2,
         setup/0]).

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

setup() ->
    {ok, Here} = file:get_cwd(),
    {ok, Host} = inet:gethostname(),

    file:make_dir(home()),
    file:make_dir(home("config")),
    file:make_dir(home("config/login")),
    file:make_dir(home("config/char")),
    file:make_dir(home("config/char/char@" ++ Host)),
    file:make_dir(home("config/zone")),
    file:make_dir(home("config/zone/zone@" ++ Host)),

    {ok, Api} = file:open(home("config/api.erl"), write),
    io:format(Api, "~w.~n~w.~n", [{port, 8000}, {key, "change-me-you-fool"}]),
    file:close(Api),

    {ok, Login} = file:open(home("config/login/server.erl"), write),

    io:format(Login, "~w.~n~w.~n~w.~n~w.~n",
        [{host, {list_to_atom(Host), login}},
         {ip, {127,0,0,1}},
         {port, 6900},
         {aliter, Here}]),

    file:close(Login),

    {ok, Char} = file:open(home("config/char/char@" ++ Host ++ "/server.erl"), write),

    io:format(Char, "~w.~n~w.~n~w.~n~w.~n~w.~n~w.~n~w.~n~w.~n",
        [{name, "Aliter"},
         {host, {list_to_atom(Host), char}},
         {ip, {127,0,0,1}},
         {port, 5121},
         {aliter, Here},
         {zone, list_to_atom("zone@" ++ Host)},
         {maintenance, 0},
         {new, 0}]),

    file:close(Char),

    {ok, Zone} = file:open(home("config/zone/zone@" ++ Host ++ "/server.erl"), write),

    io:format(Zone, "~w.~n~w.~n~w.~n~w.~n~w.~n",
        [{host, {list_to_atom(Host), zone}},
         {ip, {127,0,0,1}},
         {aliter, Here},
         {char, list_to_atom("char@" ++ Host)},
         {zones,
          [{6121,
               ["prontera",
                "izlude",
                "prt_monk",
                "prt_fild00",
                "prt_fild01",
                "prt_fild02",
                "prt_fild03",
                "prt_fild04",
                "prt_fild05",
                "prt_fild06",
                "prt_fild07",
                "prt_fild08",
                "prt_fild09",
                "prt_fild10",
                "prt_fild11",
                "new_1-1"]},
            {6122,
               ["geffen",
                "gef_fild00",
                "gef_fild01",
                "gef_fild02",
                "gef_fild03",
                "gef_fild04",
                "gef_fild05",
                "gef_fild06",
                "gef_fild07",
                "gef_fild08",
                "gef_fild09",
                "gef_fild10",
                "gef_fild11",
                "gef_fild12",
                "gef_fild13",
                "gef_fild14"]},
            {6123,
               ["payon",
                "pay_arche",
                "pay_fild01",
                "pay_fild02",
                "pay_fild03",
                "pay_fild04",
                "pay_fild05",
                "pay_fild06",
                "pay_fild07",
                "pay_fild08",
                "pay_fild09",
                "pay_fild10",
                "pay_fild11"]},
            {6124,
               ["morocc",
                "moc_fild01",
                "moc_fild02",
                "moc_fild03",
                "moc_fild04",
                "moc_fild05",
                "moc_fild06",
                "moc_fild07",
                "moc_fild08",
                "moc_fild09",
                "moc_fild10",
                "moc_fild11",
                "moc_fild12",
                "moc_fild13",
                "moc_fild14",
                "moc_fild15",
                "moc_fild16",
                "moc_fild17",
                "moc_fild18",
                "moc_fild19",
                "moc_fild20",
                "moc_fild21",
                "moc_fild22",
                "moc_ruins"]}]}]),

    file:close(Zone),

    io:format("All done!~n"),
    io:format("You may want to go through ~~/.aliter/config and tweak things.~n"),
    io:format("When you're finished, run `make install`.~n").
