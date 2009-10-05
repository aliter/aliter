-module(login_config).

-export([load/1]).

load(main) ->
    % {ok, Cwd} = file:get_cwd(),
    % load(main, Cwd ++ "/config/"),
    load(main, misc:home()).
load(main, Path) ->
    {ok, Main} = file:consult(Path ++ "main.erl"),

    lists:foreach(fun
                      ({servers, {{port, Port}, {node, Node}, {aliter, Aliter}, Servers}}) ->
                          application:set_env(login, port, Port),
                          application:set_env(login, node, Node),
                          application:set_env(login, aliter, Aliter),
                          application:set_env(login, servers, Servers);
                      ({_Key, _Val}) ->
                          ok
                  end,
                  Main).

