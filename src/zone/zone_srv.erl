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

    MapServers = lists:map(fun(M) ->
                               {ok, MapServer} = zone_map:start_link(M),
                               {M#map.name, MapServer}
                           end, Maps),

    gen_server_tcp:start_link({local, zone_master:server_for(Port)},
                              ?MODULE,
                              #state{port = Port,
                                     maps = MapServers},
                              []).

init(State) ->
    {ok, {State#state.port, zone_fsm, zone_packets:new(24)}, State}.

handle_call({provides, Map}, _From, State = #state{port = Port, maps = Maps}) ->
    case lists:dropwhile(fun({Name, _Server}) ->
                             Name /= Map
                         end, Maps) of
        [] -> {reply, no, State};
        _Yes -> {reply, {yes, Port}, State}
    end;
handle_call({add_player, Map, Player}, _From, State) ->
    {Map, MapServer} = proplists:lookup(Map, State#state.maps),

    gen_server:cast(MapServer, {add_player, Player}),

    {reply, {ok, MapServer}, State};
handle_call({get_player, ActorID}, _From, State = #state{maps = Maps}) ->
    log:debug("Zone server got get_player call.",
              [{actor, ActorID}]),
    {reply, get_player(ActorID, Maps), State};
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


get_player(_ActorID, []) ->
    none;
get_player(ActorID, [{_Name, MapServer} | Maps]) ->
    case gen_server:call(MapServer, {get_player, ActorID}) of
        {ActorID, FSM} ->
            {ok, FSM};
        none ->
            get_player(ActorID, Maps)
    end.
