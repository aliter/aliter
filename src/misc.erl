-module(misc).

-export([home/0,
         md5_hash/1]).

home() ->
    {ok, [[Home]]} = init:get_argument(home),
    Home ++ "/.aliter/".

md5_hash(S) ->
    Md5 = erlang:binary_to_list(erlang:md5(S)),
    lists:flatten(list_to_hex(Md5)).

list_to_hex(L) ->
    lists:map(fun(I) -> int_to_hex(I) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).
