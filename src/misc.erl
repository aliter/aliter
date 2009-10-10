-module(misc).

-export([home/0]).

home() ->
    {ok, [[Home]]} = init:get_argument(home),
    Home ++ "/.aliter/".

