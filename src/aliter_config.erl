-module(aliter_config).

-export([load/1]).

load(main) ->
    {ok, Cwd} = file:get_cwd(),
    load(main, Cwd ++ "/config/"),
    load(main, aliter:home()).
load(main, Path) ->
    {ok, Main} = file:consult(Path ++ "main.erl"),

    lists:foreach(fun({Name, Val}) ->
                      application:set_env(aliter, Name, Val)
                  end, Main).
