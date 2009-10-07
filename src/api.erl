-module(api).
-behaviour(gen_listener_tcp).

-include("include/records.hrl").

-export([start_link/0,
         init/1,
         handle_accept/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([dc/1]).

start_link() ->
    {ok, API} = application:get_env(aliter, api),
    {port, Port} = proplists:lookup(port, API),
    {key, Key} = proplists:lookup(key, API),

    log:info("Starting API.", [{port, Port}]),

    gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, {Port, Key}, []).

init({Port, Key}) ->
    {ok, {Port, [binary, {packet, raw}, {active, false}]}, Key}.

loop(Socket, Key) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            R = api_pb:decode_apirequest(Data),

            case R#apirequest.key of
                Key ->
                    gen_server_tcp:cast({server, list_to_atom(R#apirequest.node)}, {R, self()}),
                    log:debug("API got request.", [{request, R}]);
                _Invalid ->
                    log:warning("API got request with invalid key.", [{request, R}])
            end,

            ?MODULE:loop(Socket, Key);

        {tcp_closed, Socket} ->
            log:info("Client disconnected from API.");

        Packet when is_binary(Packet) ->
            log:debug("Sending API response.", [{response, api_pb:decode_apiresponse(Packet)}]),
            gen_tcp:send(Socket, Packet),
            ?MODULE:loop(Socket, Key)
    end.

handle_accept(Socket, Key) ->
    log:info("Client connected to API.", [{client, element(2, inet:peername(Socket))}]),

    Pid = spawn(fun() -> loop(Socket, Key) end),
    gen_tcp:controlling_process(Socket, Pid),
    {noreply, Key}.

handle_call(Request, _From, Key) ->
    log:debug("API got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, Key}.

handle_cast(Cast, Key) ->
    log:debug("API got cast.", [{cast, Cast}]),
    {noreply, Key}.

handle_info(Info, Key) ->
    log:debug("API got info.", [{info, Info}]),
    {noreply, Key}.

terminate(_Reason, _Key) ->
    log:info("API terminating."),
    ok.

code_change(_OldVsn, Key, _Extra) ->
    {ok, Key}.


% Prepare a record for mnesia:match_object.
dc(undefined) ->
    '_';
dc(V) when is_tuple(V) ->
    list_to_tuple(lists:map(fun dc/1, tuple_to_list(V)));
dc(Any) ->
    Any.

