-module(zone_srv).
-behaviour(gen_server_tcp).

-include("include/records.hrl").

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state,
        {port,
         maps}).


start_link(Port, Maps) ->
    log:info("Starting zone server.", [{port, Port}]),

    gen_server_tcp:start_link(?MODULE, {Port, Maps}, []).

init({Port, Maps}) ->
    {ok, {Port, zone_fsm, zone_packets}, #state{port = Port, maps = Maps}}.

handle_call({provides, Map}, _From, State = #state{port = Port, maps = Maps}) ->
    case lists:dropwhile(fun(M) -> M#map.name /= Map end, Maps) of
        [] -> {reply, no, State};
        _Yes -> {reply, {yes, Port}, State}
    end;
handle_call(Request, _From, State) ->
    log:debug("Zone server got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, State}.

handle_cast(Cast, State) ->
    log:debug("Zone server got cast.", [{cast, Cast}]),
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    log:debug("Zone server got EXIT signal.", [{reason, Reason}]),
    {stop, normal, State};
handle_info(Info, State) ->
    log:debug("Zone server got info.", [{info, Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    log:info("Zone server terminating."),
    mnesia:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
