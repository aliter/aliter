-module(char_fsm).
-behaviour(gen_fsm).

-include("include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start_link/1]).

-export([handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([init/1,
         locked/2,
         locked/3,
         valid/2,
         valid/3,
         renaming/2,
         renaming/3,
         chosen/2,
         chosen/3]).

-define(MAX_SLOTS, 9).
-define(AVAILABLE_SLOTS, 9).
-define(PREMIUM_SLOTS, 9).


start_link(Tcp) ->
    gen_fsm:start_link(?MODULE, Tcp, []).

init(Tcp) ->
    {ok, locked, #char_state{tcp=Tcp}}.

locked({connect, AccountID, LoginIDa, LoginIDb, _Gender}, State) ->
    State#char_state.tcp ! <<AccountID:32/little>>,

    {node, LoginNode} = config:get_env(char, login.node),
    Verify = gen_server_tcp:call({server, LoginNode},
                                 {verify_session,
                                  AccountID,
                                  LoginIDa,
                                  LoginIDb}),

    log:info("Character connect request.",
             [{account, AccountID},
              {ids, {LoginIDa, LoginIDb}},
              {verified, Verify}]),

    case Verify of
        {ok, FSM} ->
            {ok, L} = gen_fsm:sync_send_event(FSM, switch_char),

            log:debug("Switched to Character server.",
                      [{login_state, L}]),

            gen_server_tcp:cast(server,
                                {add_session, {AccountID,
                                               self(),
                                               L#login_state.id_a,
                                               L#login_state.id_b}}),

            GetChars = fun() ->
                           qlc:e(qlc:q([X || X <- mnesia:table(char),
                                             X#char.account_id =:= AccountID]))
                       end,
            {atomic, Chars} = mnesia:transaction(GetChars),

            State#char_state.tcp ! {parse, char_packets:new(L#login_state.packet_ver)},
            State#char_state.tcp ! {characters, {Chars, ?MAX_SLOTS, ?AVAILABLE_SLOTS, ?PREMIUM_SLOTS}},

            {next_state, valid, State#char_state{account = L#login_state.account,
                                                 id_a = L#login_state.id_a,
                                                 id_b = L#login_state.id_b,
                                                 packet_ver = L#login_state.packet_ver}};
        invalid ->
            State#char_state.tcp ! {refuse, 0},
            {next_state, locked, State}
    end;
locked(Event, State) ->
    ?MODULE:handle_event(Event, locked, State).

locked(Event, From, State) ->
    handle_sync_event(Event, From, locked, State).


valid({choose, Num}, State = #char_state{account = #account{id = AccountID}}) ->
    GetChar = fun() ->
                  qlc:e(qlc:q([X || X <- mnesia:table(char),
                                    X#char.num =:= Num,
                                    X#char.account_id =:= AccountID]))
              end,

    case mnesia:transaction(GetChar) of
        {atomic, [C]} ->
            log:debug("Player selected character.", [{account, AccountID}, {character, C#char.id}]),

            {ip, ZoneIP} = config:get_env(char, zone.server.ip),
            {zone, ZoneNode} = config:get_env(char, server.zone),

            {zone, ZonePort, _ZoneServer}
                = gen_server:call({zone_master, ZoneNode},
                                  {who_serves, C#char.map}),

            State#char_state.tcp ! {zone_connect,
                                    {C,
                                     ZoneIP,
                                     ZonePort}},

            {next_state, chosen, State#char_state{char = C}};
        {atomic, []} ->
            log:warning("Player selected invalid character.",
                        [{account, AccountID}]),
            State#char_state.tcp ! {refuse, 1},
            {next_state, valid, State};
        Error ->
            log:warning("Error grabbing selected character.",
                        [{result, Error}]),
            State#char_state.tcp ! {refuse, 1},
            {next_state, valid, State}
    end;
valid({create, Name, Str, Agi, Vit, Int, Dex, Luk, Num, HairColour, HairStyle},
      State = #char_state{account = Account}) ->
    Create = fun() ->
                  case qlc:e(qlc:q([C || C <- mnesia:table(char),
                                         C#char.name =:= Name])) of
                      [] ->
                          Char = #char{id = mnesia:dirty_update_counter(ids, char, 0),
                                       num = Num,
                                       name = Name,
                                       zeny = 500, % TODO: Config flag
                                       str = Str,
                                       agi = Agi,
                                       vit = Vit,
                                       int = Int,
                                       dex = Dex,
                                       luk = Luk,
                                       hair_colour = HairColour,
                                       hair_style = HairStyle,
                                       account_id = Account#account.id},
                          mnesia:dirty_update_counter(ids, char, 1),
                          mnesia:write(Char),
                          Char;
                      _Exists ->
                          exists
                  end
             end,

    case mnesia:transaction(Create) of
        {atomic, Char} when is_record(Char, char)->
            log:info("Created character.",
                     [{account, Account},
                      {char, Char}]),
            State#char_state.tcp ! {character_created, Char};
        {atomic, exists} ->
            log:info("Character creation denied (name already in use).",
                     [{account, Account}]),
            State#char_state.tcp ! {creation_failed, 0};
        Error ->
            log:info("Character creation failed.",
                     [{account, Account},
                      {result, Error}]),
            State#char_state.tcp ! {creation_failed, 16#FF}
    end,

    {next_state, valid, State};
valid({delete, CharacterID, EMail}, State = #char_state{account = #account{id = AccountID, email = AccountEMail}}) ->
    case EMail of
        AccountEMail ->
            Delete = fun() ->
                         [Char] = qlc:e(qlc:q([C || C <- mnesia:table(char),
                                                    C#char.id =:= CharacterID,
                                                    C#char.account_id =:= AccountID])),
                         mnesia:delete_object(Char),
                         Char
                     end,

            case mnesia:transaction(Delete) of
                {atomic, Char} ->
                    log:info("Character deleted.",
                             [{char, Char}]),
                    State#char_state.tcp ! {character_deleted, ok};
                Error ->
                    log:warning("Character deletion failed.",
                                [{char_id, CharacterID},
                                 {account_id, AccountID},
                                 {email, EMail},
                                 {result, Error}]),
                    State#char_state.tcp ! {deletion_failed, 0}
            end;
        _Invalid ->
            log:warning("Character deletion attempted with wrong e-mail address.",
                        [{email, EMail},
                         {wanted, AccountEMail}]),
            State#char_state.tcp ! {deletion_failed, 0}
    end,
    {next_state, valid, State};
valid({check_name, AccountID, CharacterID, NewName}, State = #char_state{account = #account{id = AccountID}}) ->
    Check = fun() ->
                [] = qlc:e(qlc:q([C || C <- mnesia:table(char),
                                       C#char.name =:= NewName])),
                qlc:e(qlc:q([C || C <- mnesia:table(char),
                                  C#char.id =:= CharacterID,
                                  C#char.account_id =:= AccountID]))
            end,

    case mnesia:transaction(Check) of
        {atomic, [Char]} ->
            State#char_state.tcp ! {name_check_result, 1},
            {next_state, renaming, State#char_state{rename = {Char, NewName}}};
        _Invalid ->
            State#char_state.tcp ! {name_check_result, 0},
            {next_state, valid, State}
    end;
valid(Event, State) ->
    ?MODULE:handle_event(Event, valid, State).

valid(Event, From, State) ->
    handle_sync_event(Event, From, valid, State).


chosen(stop, State) ->
    log:debug("Character FSM waiting 5 minutes to exit."),
    {next_state,
     valid,
     State#char_state{die = gen_fsm:send_event_after(5 * 60 * 1000, exit)}};
chosen(exit, State) ->
    {stop, normal, State};
chosen(Event, State) ->
    ?MODULE:handle_event(Event, chosen, State).

chosen(Event, From, State) ->
    handle_sync_event(Event, From, chosen, State).


renaming({rename, CharacterID},
         State = #char_state{rename = {Char = #char{id = CharacterID,
                                               renamed = 0},
                                  NewName}}) ->
    Write = fun() ->
                [] = qlc:e(qlc:q([C || C <- mnesia:table(char),
                                       C#char.name =:= NewName])),
                mnesia:write(Char#char{name = NewName, renamed = 1})
            end,

    case mnesia:transaction(Write) of
        {atomic, ok} ->
            State#char_state.tcp ! {rename_result, 0};
        _Error ->
            State#char_state.tcp ! {rename_result, 3}
    end,

    {next_state, valid, State#char_state{rename = undefined}};
renaming({rename, _CharacterID},
         State = #char_state{rename = {#char{renamed = 1}, _NewName}}) ->
    State#char_state.tcp ! {rename_result, 1},
    {next_state, valid, State};
renaming(Event, State) ->
    log:debug("Character FSM received invalid event.",
              [{event, Event},
               {state, renaming}]),
    {next_state, renaming, State}.

renaming(Event, From, State) ->
    handle_sync_event(Event, From, renaming, State).


handle_event(stop, _StateName, StateData) ->
    log:info("Character FSM stopping."),
    {stop, normal, StateData};
handle_event({set_server, Server}, StateName, StateData) ->
    {next_state, StateName, StateData#char_state{server = Server}};
handle_event({update_state, Fun}, StateName, StateData) ->
    {next_state, StateName, Fun(StateData)};
handle_event(Event, StateName, StateData) ->
    log:debug("Character FSM got event.", [{event, Event}, {state, StateName}, {state_data, StateData}]),
    {next_state, StateName, StateData}.

handle_sync_event(switch_zone, _From, StateName, StateData) ->
    gen_fsm:cancel_timer(StateData#char_state.die),
    {reply, {ok, StateData}, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    log:debug("Character FSM got sync event."),
    {next_state, StateName, StateData}.

handle_info({'EXIT', _From, Reason}, _StateName, StateData) ->
    log:debug("Character FSM got EXIT signal.", [{reason, Reason}]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    log:debug("Character FSM got info.", [{info, Info}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #char_state{account = #account{id = AccountID}}) ->
    log:debug("Character FSM terminating.",
              [{account, AccountID}]),

    gen_server_tcp:cast(server, {remove_session, AccountID}),

    {node, LoginNode} = config:get_env(char, login.node),
    gen_server_tcp:cast({server, LoginNode},
                        {remove_session, AccountID});
terminate(_Reason, _StateName, _StateData) ->
    log:debug("Character FSM terminating."),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
