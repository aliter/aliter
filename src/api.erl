-module(api).
-behaviour(gen_listener_tcp).

-include("include/records.hrl").

-export([start_link/0,
         init/1,
         loop/1,
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

    log:info("Starting API.", [{port, Port}]),

    gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, {Port, [binary, {packet, raw}, {active, false}]}, []}.

loop(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            R = api_pb:decode_apirequest(Data),

            log:debug("API got request.", [{request, R}]),

            gen_server_tcp:cast({server, list_to_atom(R#apirequest.node)}, {R, self()}),

            ?MODULE:loop(Socket);

        {tcp_closed, Socket} ->
            log:info("Client disconnected from API.");

        Packet when is_binary(Packet) ->
            log:debug("Sending API response.", [{response, api_pb:decode_apiresponse(Packet)}]),
            gen_tcp:send(Socket, Packet),
            ?MODULE:loop(Socket)
    end.

handle_accept(Socket, St) ->
    log:info("Client connected to API.", [{client, element(2, inet:peername(Socket))}]),

    Pid = spawn(fun() ->
                        loop(Socket)
                end),
    gen_tcp:controlling_process(Socket, Pid),
    {noreply, St}.

handle_call(Request, _From, Config) ->
    log:debug("API got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, Config}.

handle_cast(Cast, Config) ->
    log:debug("API got cast.", [{cast, Cast}]),
    {noreply, Config}.

handle_info(Info, Config) ->
    log:debug("API got info.", [{info, Info}]),
    {noreply, Config}.

terminate(_Reason, _Config) ->
    log:info("API terminating."),
    ok.

code_change(_OldVsn, Config, _Extra) ->
    {ok, Config}.


% Prepare a record for mnesia:match_object.
dc(undefined) ->
    '_';
dc(V) when is_tuple(V) ->
    list_to_tuple(lists:map(fun dc/1, tuple_to_list(V)));
dc(Any) ->
    Any.

