-module(login_config).

-export([load/1]).

load(main) ->
    % {ok, Cwd} = file:get_cwd(),
    % load(main, Cwd ++ "/config/"),
    load(main, misc:home()).
load(main, Path) ->
    {ok, Main} = file:consult(Path ++ "main.erl"),

    lists:foreach(fun
                      ({servers, {LoginConf, Servers}}) ->
                          setFrom(port, LoginConf),
                          setFrom(node, LoginConf),
                          setFrom(aliter, LoginConf),
                          application:set_env(login, servers, Servers);
                      ({_Key, _Val}) ->
                          ok
                  end,
                  Main).

setFrom(Key, Conf) ->
    application:set_env(login, Key, proplists:get_value(Key, Conf)).
