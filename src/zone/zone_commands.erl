-module(zone_commands).

-include("include/records.hrl").

-export([parse/1,
         execute/4]).

parse(String) ->
    string:tokens(String, " ").

execute(_FSM, "caps", Args,
        State = #zone_state{tcp = TCP,
                            map_server = MapServer,
                            account = #account{id = AccountID},
                            char = #char{id = CharacterID,
                                         x = X,
                                         y = Y}}) ->
    Capitalized = string:to_upper(string:join(Args, " ")),

    gen_server:cast(MapServer,
                    {send_to_other_players_in_sight,
                     {X, Y},
                     CharacterID,
                     actor_message,
                     {AccountID, Capitalized}}),

    TCP ! {message, Capitalized},

    {ok, State};
execute(FSM, "load", _Args, State = #zone_state{char = C}) ->
    log:error("Got load command."),

    warp_to(FSM, C#char.save_map, C#char.save_x, C#char.save_y, State);
execute(FSM, "warp", [Map | [XStr | [YStr | _]]], State) ->
    log:error("Got warp command."),

    case {string:to_integer(XStr),
          string:to_integer(YStr)} of
        {{X,_}, {Y,_}} when is_integer(X), is_integer(Y) ->
            warp_to(FSM, Map, X, Y, State);
        _Invalid ->
            zone_fsm:say("Invalid coordinates.", State),
            {ok, State}
    end;
execute(FSM, "jumpto", [PlayerName | _], State) ->
    case gen_server:call(zone_master,
                         {get_player_by,
                          fun(#zone_state{char = C}) ->
                              C#char.name == PlayerName
                          end}) of
        {ok, #zone_state{char = C}} ->
            warp_to(FSM, C#char.map, C#char.x, C#char.y, State);
        none ->
            zone_fsm:say("Player not found.", State)
    end;
execute(_FSM, Unknown, Args, State) ->
    log:warning("Unknown command.",
                [{command, Unknown},
                 {args, Args}]),

    zone_fsm:say("Unknown command `" ++ Unknown ++ "'.", State),

    ok.


warp_to(FSM, Map, X, Y,
        State = #zone_state{tcp = TCP,
                            map_server = MapServer,
                            account = #account{id = AccountID},
                            char = C}) ->
    case gen_server:call(zone_master,
                         {who_serves, Map}) of
        {zone, Port, ZoneServer} ->
            {ip, IP} = config:get_env(zone, server.ip),

            zone_fsm:say(lists:concat(["Warped to ", Map, " (", X, ", ", Y, ")."]),
                         State),

            % Send warp packets
            gen_server:cast(MapServer,
                            {send_to_other_players_in_sight,
                             {C#char.x, C#char.y},
                             C#char.id,
                             vanish,
                             {AccountID, 3}}),

            % Move to new Map server
            gen_server_tcp:cast(MapServer, {remove_player, AccountID}),
            {ok, NewMap, NewMapServer}
                = gen_server_tcp:call(ZoneServer,
                                      {add_player,
                                       Map,
                                       {AccountID, FSM}}),

            NewStateFun = fun(St) ->
                              St#zone_state{server = ZoneServer,
                                            map = NewMap,
                                            map_server = NewMapServer,
                                            char = C#char{map = Map,
                                                          x = X,
                                                          y = Y}}
                          end,

            zone_fsm:show_actors(NewStateFun(State)),

            gen_fsm:send_all_state_event(FSM,
                                         {update_state,
                                          NewStateFun}),

            TCP ! {warp_zone, {Map, X, Y, IP, Port}};
        none ->
            zone_fsm:say("Invalid map provided.", State),
            ok
    end.
