-module(zone_map).
-behaviour(gen_server).

-include("include/records.hrl").

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(map_state,
        {map,
         players = [],
         npcs = [],
         mobs = []}).


start_link(Map) ->
    log:debug("Zone map server starting.",
              [{map, Map#map.name}]),

    gen_server:start_link({local, list_to_atom("zone_map_" ++ Map#map.name)},
                          ?MODULE,
                          #map_state{map = Map},
                          []).

init(State) ->
    {ok, State}.

handle_call({get_player, Player}, _From, State = #map_state{players = Players}) ->
    {reply, proplists:lookup(Player, Players), State};
handle_call(Request, _From, State) ->
    log:debug("Zone map server got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, State}.

handle_cast({add_player, Player}, State = #map_state{players = Players}) ->
    log:debug("Zone map server adding player.",
              [{player, Player}]),

    {noreply, State#map_state{players = [Player | Players]}};
handle_cast({send_to_players, Packet, Data}, State) ->
    lists:foreach(fun({_ID, FSM}) ->
                      gen_fsm:send_all_state_event(FSM,
                                                   {send_packet,
                                                    Packet,
                                                    Data})
                  end,
                  State#map_state.players),

    {noreply, State};
handle_cast({send_to_other_players, Self, Packet, Data}, State) ->
    lists:foreach(fun({_ID, FSM}) ->
                      gen_fsm:send_all_state_event(FSM,
                                                   {send_packet_if,
                                                    fun(#zone_state{char = C}) ->
                                                        (C#char.id /= Self)
                                                    end,
                                                    Packet,
                                                    Data})
                  end,
                  State#map_state.players),

    {noreply, State};
handle_cast({send_to_players_in_sight, {X, Y}, Packet, Data}, State) ->
    lists:foreach(fun({_ID, FSM}) ->
                      gen_fsm:send_all_state_event(FSM,
                                                   {send_packet_if,
                                                    fun(#zone_state{char = C}) ->
                                                        in_range(C, {X, Y})
                                                    end,
                                                    Packet,
                                                    Data})
                  end,
                  State#map_state.players),

    {noreply, State};
handle_cast({send_to_other_players_in_sight, {X, Y}, Self, Packet, Data}, State) ->
    lists:foreach(fun({_ID, FSM}) ->
                      gen_fsm:send_all_state_event(FSM,
                                                   {send_packet_if,
                                                    fun(#zone_state{char = C}) ->
                                                        (C#char.id /= Self) and
                                                        in_range(C, {X, Y})
                                                    end,
                                                    Packet,
                                                    Data})
                  end,
                  State#map_state.players),

    {noreply, State};
handle_cast(Cast, State) ->
    log:debug("Zone map server got cast.", [{cast, Cast}]),
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    log:debug("Zone map server got EXIT signal.", [{reason, Reason}]),
    {stop, normal, State};
handle_info(Info, State) ->
    log:debug("Zone map server got info.", [{info, Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    log:info("Zone map server terminating."),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


in_range(C, {X, Y}) ->
    (((C#char.x - X) < 17) and
     ((C#char.x - X) > -17) and
     ((C#char.y - Y) < 17) and
     ((C#char.y - Y) > -17)).
