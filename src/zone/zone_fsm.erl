-module(zone_fsm).
-behaviour(gen_fsm).

-include("include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start_link/1]).

-export([handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([init/1, locked/2, valid/2]).


start_link(Tcp) ->
    gen_fsm:start_link(?MODULE, Tcp, []).

init(Tcp) ->
    {ok, locked, #zone_state{tcp=Tcp}}.

locked({connect, AccountID, CharacterID, SessionIDa, Gender}, State) ->
    {char, CharNode} = config:get_env(zone, server.char),
    Session = gen_server_tcp:call({server, CharNode},
                                  {verify_session, AccountID, CharacterID, SessionIDa}),

    case Session of
        {ok, FSM} ->
            {ok, C} = gen_fsm:sync_send_event(FSM, switch_zone),

            log:debug("Switched to Zone server.",
                      [{char_state, C}]),

            {ok, MapServer} = gen_server_tcp:call(State#zone_state.server,
                                                  {add_player,
                                                   (C#char_state.char)#char.map,
                                                   {(C#char_state.account)#account.id, self()}}),

            State#zone_state.tcp ! {parse, zone_packets:new(C#char_state.packet_ver)},

            State#zone_state.tcp ! {16#283, SessionIDa},
            State#zone_state.tcp ! {16#73, {zone_master:tick(),
                                            {(C#char_state.char)#char.x,
                                             (C#char_state.char)#char.y, 0}}},

            {next_state, valid, State#zone_state{map_server = MapServer,
                                                 account = C#char_state.account,
                                                 char = C#char_state.char,
                                                 id_a = C#char_state.id_a,
                                                 id_b = C#char_state.id_b,
                                                 packet_ver = C#char_state.packet_ver}};
        invalid ->
            log:warning("Invalid zone login attempt caught.",
                        [{account_id, AccountID},
                         {character_id, CharacterID}]),
            {next_state, locked, State}
    end;
locked(Event, State) ->
    ?MODULE:handle_event(Event, locked, State).

valid({request_name, ActorID}, State = #zone_state{account = #account{id = AccountID},
                                                   char = #char{name = CharacterName}}) ->
    log:debug("Sending actor name.", [{actor, ActorID}]),

    Name = if
               ActorID == AccountID ->
                   CharacterName;
               true ->
                   {ok, FSM} = gen_server:call(State#zone_state.map_server,
                                               {get_player, ActorID}),

                   {ok, Z} = gen_fsm:sync_send_all_state_event(FSM, get_state),
                   (Z#zone_state.char)#char.name
           end,

    State#zone_state.tcp ! {16#95, {ActorID, Name}},

    {next_state, valid, State};
valid(map_loaded, State) ->
    {next_state, valid, State};
valid(request_guild_status,
      State = #zone_state{char = #char{id = CharacterID,
                                       guild_id = GuildID}}) when GuildID /= 0 ->
    log:debug("Requested guild status."),

    GetGuild = fun() ->
                   mnesia:read(guild, GuildID)
               end,
    case mnesia:transaction(GetGuild) of
        {atomic, [#guild{master_id = CharacterID}]} ->
            State#zone_state.tcp ! {16#4e, master};
        {atomic, [_G]} ->
            State#zone_state.tcp ! {16#4e, member};
        _Error ->
            ok
    end,

    {next_state, valid, State};
valid({request_guild_info, 0},
      State = #zone_state{char = #char{guild_id = GuildID}}) when GuildID /= 0 ->
    log:debug("Requested first page of guild info."),

    GetGuild = fun() -> mnesia:read(guild, GuildID) end,
    case mnesia:transaction(GetGuild) of
        {atomic, [G]} ->
            State#zone_state.tcp ! {16#1b6, G},
            State#zone_state.tcp ! {16#14c, G#guild.relationships};
        _Other ->
            ok
    end,

    {next_state, valid, State};
valid({request_guild_info, 1},
      State = #zone_state{char = #char{guild_id = GuildID}}) when GuildID /= 0 ->
    log:debug("Requested second page of guild info."),

    {char, CharNode} = config:get_env(zone, server.char),

    GetMembers = gen_server_tcp:call({server, CharNode},
                                     {get_chars,
                                      fun() ->
                                          qle:e(qlc:q([C || C <- mnesia:table(char),
                                                            C#char.guild_id == GuildID]))
                                      end}),
    case GetMembers of
        {atomic, Members} ->
            State#zone_state.tcp ! {16#154, Members};
        _Error ->
            ok
    end,

    {next_state, valid, State};
valid({request_guild_info, 2}, State) ->
    log:debug("Requested third page of guild info."),
    {next_state, valid, State};
valid(Event, State) ->
    ?MODULE:handle_event(Event, valid, State).

handle_event({speak, Message},
             StateName,
             StateData = #zone_state{tcp = TCP,
                                     map_server = MapServer,
                                     account = #account{id = AccountID},
                                     char = #char{x = X, y = Y}}) ->
    log:debug("Speaking.", [{message, Message}]),

    gen_server:cast(MapServer,
                    {send_to_other_players_in_sight,
                     {X, Y},
                     16#8d,
                     {AccountID, Message}}),

    TCP ! {16#8e, Message},

    {next_state, StateName, StateData};
handle_event(stop, _StateName, StateData) ->
    log:info("Zone FSM stopping."),
    {stop, normal, StateData};
handle_event({tick, Tick}, StateName, StateData) when StateName /= locked ->
    log:debug("Got tick; syncing.", [{tick, Tick}]),
    StateData#zone_state.tcp ! {16#7f, zone_master:tick()},
    {next_state, StateName, StateData};
handle_event({set_server, Server}, StateName, StateData) ->
    {next_state, StateName, StateData#zone_state{server = Server}};
handle_event({send_packet, Packet, Data}, StateName, StateData) ->
    StateData#zone_state.tcp ! {Packet, Data},
    {next_state, StateName, StateData};
handle_event({send_packet_if, Pred, Packet, Data}, StateName, StateData) ->
    case Pred(StateData) of
        true ->
            StateData#zone_state.tcp ! {Packet, Data};
        false ->
            ok
    end,

    {next_state, StateName, StateData};
handle_event(Event, StateName, StateData) ->
    log:debug("Zone FSM received event.", [{event, Event}, {state, StateName}]),
    {next_state, StateName, StateData}.

handle_sync_event(get_state, _From, StateName, StateData) ->
    log:debug("Handling get_state handle_sync_event."),
    {reply, {ok, StateData}, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    log:debug("Zone FSM got sync event."),
    {next_state, StateName, StateData}.

handle_info({'EXIT', _From, Reason}, _StateName, StateData) ->
    log:debug("Zone FSM got EXIT signal.", [{reason, Reason}]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    log:debug("Zone FSM got info.", [{info, Info}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #zone_state{account = #account{id = AccountID}}) ->
    log:info("Zone FSM terminating.",
            [{account, AccountID}]),

    gen_server_tcp:cast(server, {remove_session, AccountID});
terminate(_Reason, _StateName, _StateData) ->
    log:debug("Zone FSM terminating."),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
