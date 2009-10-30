-module(c_interface).

-include("include/records.hrl").

-export([start/0, stop/0]).

-export([pathfind/3]).


pathfind(M, {X, Y}, {ToX, ToY}) ->
    log:debug("Pathfinding to C.",
              [{map, {M#map.name, M#map.id, M#map.width, M#map.height}},
               {size, bit_size(M#map.cells)}]),

    call_node({pathfind,
               {M#map.id, X, Y, ToX, ToY}}).


start() ->
    register(cnode,
             spawn(fun() ->
                       os:cmd(lists:concat(["priv/extern", " ", node(), " ", erlang:get_cookie()]))
                   end)).

stop() ->
    exit(cnode, normal).

call_node(Msg) ->
    {host, {Host, _Name}} = config:get_env(zone, server.host),
    CNode = list_to_atom(lists:concat(["c1@", Host])),

    {any, CNode} ! {call, self(), Msg},
    receive
        {data, Result} ->
            {ok, Result}
    after
        10000 ->
            log:warning("C interface timed out."),
            {error, timeout}
    end.

