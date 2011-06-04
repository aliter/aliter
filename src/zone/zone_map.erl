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


start_link(Map) ->
    gen_server:start_link({local, list_to_atom("zone_map_" ++ Map#map.name)},
                          ?MODULE,
                          #map_state{map = Map},
                          []).

init(State) ->
    {ok, State}.

handle_call({get_actor, ActorID},
            _From,
            State = #map_state{players = Players,
                               npcs = NPCs}) ->
    case proplists:lookup(ActorID, Players) of
        {ActorID, FSM} ->
            {reply, {player, FSM}, State};
        none ->
            case lists:keyfind(ActorID, 2, NPCs) of
                none ->
                    {reply, none, State};
                NPC ->
                    {reply, {npc, NPC}, State}
            end
    end;
handle_call({get_player_by, Pred}, _From, State = #map_state{players = Players}) ->
    {reply, get_player_by(Pred, Players), State};
handle_call(player_count, _From, State = #map_state{players = Players}) ->
    {reply, length(Players), State};
handle_call(Request, _From, State) ->
    log:debug("Zone map server got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, State}.

handle_cast({add_player, Player}, State = #map_state{players = Players}) ->
    log:debug("Zone map server adding player.",
              [{player, Player}]),

    {noreply, State#map_state{players = [Player | Players]}};
handle_cast({register_npc, NPC}, State = #map_state{npcs = NPCs}) ->
    log:warning("Registering NPC.", [{npc, NPC}]),
    % TODO: make it appear on screen for anyone around it
    {noreply, State#map_state{npcs = [NPC | NPCs]}};
handle_cast({remove_player, AccountID}, State = #map_state{players = Players}) ->
    log:debug("Zone map server removing player.",
              [{account, AccountID}]),

    {noreply, State#map_state{players = lists:keydelete(AccountID, 1, Players)}};
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
handle_cast({show_actors, {SelfID, SelfFSM}}, State) ->
    lists:foreach(fun
                      ({ID, _FSM}) when ID == SelfID ->
                          ok;
                      ({_ID, FSM}) ->
                          gen_fsm:send_all_state_event(FSM,
                                                       {show_to,
                                                        SelfFSM})
                  end,
                  State#map_state.players),

    lists:foreach(fun(N) ->
                      log:error("Showing NPC."),
                      gen_fsm:send_all_state_event(SelfFSM,
                                                   {send_packet,
                                                    show_npc,
                                                    N})
                  end,
                  State#map_state.npcs),

    {noreply, State};
handle_cast(Cast, State) ->
    log:debug("Zone map server got cast.", [{cast, Cast}]),
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    log:error("Zone map server got EXIT signal.", [{from, From}, {reason, Reason}]),
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

get_player_by(_Pred, []) ->
    none;
get_player_by(Pred, [{_ID, FSM} | Players]) ->
    log:debug("Looking for player from zone_map.",
              [{server, FSM},
               {pred, Pred}]),

    case gen_fsm:sync_send_all_state_event(FSM, get_state) of
        {ok, State} ->
            case Pred(State) of
                false ->
                    get_player_by(Pred, Players);
                true ->
                    {ok, State}
            end;
        _Fail ->
            get_player_by(Pred, Players)
    end.
