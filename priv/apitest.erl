-module(apitest).

-include("include/records.hrl").

-export([loop/1,
         connect/0]).

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),

    receive
        {tcp, Socket, Data} ->
            io:format("Got data: ~p~n", [Data]),
            io:format("Decoded: ~p~n", [api_pb:decode_apiresponse(Data)]),
            ?MODULE:loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Disconnected.")
    end.

connect() ->
    {ok, API} = gen_tcp:connect("localhost", 8000, [binary, {packet, raw}, {active, false}]),
    Pid = spawn(fun() -> loop(API) end),
    gen_tcp:controlling_process(API, Pid),
    API.

