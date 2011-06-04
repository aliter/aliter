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
                               {M#map.name, M, MapServer}
                           end, Maps),

    gen_server_tcp:start_link({local, zone_master:server_for(Port)},
                              ?MODULE,
                              #state{port = Port,
                                     maps = MapServers},
                              []).

init(State) ->
    {ok, {State#state.port, zone_fsm, zone_packets:new(24)}, State}.

handle_call({provides, MapName}, _From, State = #state{port = Port, maps = Maps}) ->
    case proplists:lookup(MapName, Maps) of
        none -> {reply, no, State};
        {MapName, _Map, _Server} -> {reply, {yes, Port}, State}
    end;
handle_call({add_player, MapName, Player}, _From, State) ->
    {MapName, Map, MapServer} = proplists:lookup(MapName, State#state.maps),

    gen_server:cast(MapServer, {add_player, Player}),

    {reply, {ok, Map, MapServer}, State};
handle_call({get_actor, ActorID}, _From, State = #state{maps = Maps}) ->
    log:debug("Zone server got get_actor call.",
              [{actor, ActorID}]),
    {reply, get_actor(ActorID, Maps), State};
handle_call({get_player_by, Pred}, _From, State = #state{maps = Maps}) ->
    log:debug("Zone server got get_player_by call."),
    {reply, get_player_by(Pred, Maps), State};
handle_call(player_count, _From, State = #state{maps = Maps}) ->
    log:debug("Zone server got player_count call."),
    {reply, get_player_count(Maps), State};
handle_call(Request, _From, State) ->
    log:debug("Zone server got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, State}.


handle_cast({send_to_all, Msg}, State) ->
    lists:foreach(fun({_Name, _Map, MapServer}) ->
                      gen_server:cast(MapServer,
                                      Msg)
                  end,
                  State#state.maps),
    {noreply, State};
handle_cast({register_npc, NPC = #npc{map = MapName}}, State = #state{maps = Maps}) ->
    case proplists:lookup(MapName, Maps) of
        none ->
            {noreply, State};
        {_Name, _Map, MapServer} ->
            gen_server:cast(MapServer, {register_npc, NPC}),
            {noreply, State}
    end;
handle_cast(Cast, State) ->
    log:debug("Zone server got cast.", [{cast, Cast}]),
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    log:error("Zone server got EXIT signal.", [{from, From}, {reason, Reason}]),
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


get_player_count([]) ->
    0;
get_player_count([{_Name, _Map, MapServer} | Maps]) ->
    gen_server_tcp:call(MapServer, player_count) +
        get_player_count(Maps).

get_actor(_ActorID, []) ->
    none;
get_actor(ActorID, [{_Name, _Map, MapServer} | Maps]) ->
    case gen_server:call(MapServer, {get_actor, ActorID}) of
        none ->
            get_actor(ActorID, Maps);
        Found ->
            {ok, Found}
    end.

get_player_by(_Pred, []) ->
    none;
get_player_by(Pred, [{_Name, _Map, MapServer} | Maps]) ->
    log:debug("Looking for player from zone_srv.",
              [{server, MapServer},
               {pred, Pred}]),

    case gen_server:call(MapServer, {get_player_by, Pred}) of
        {ok, State} ->
            {ok, State};
        none ->
            get_player_by(Pred, Maps)
    end.
