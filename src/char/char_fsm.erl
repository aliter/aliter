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

-export([init/1, locked/2, valid/2, renaming/2]).

-record(state, {tcp,
                account,
                rename}).

start_link(Tcp) ->
    gen_fsm:start_link(?MODULE, Tcp, []).

init(Tcp) ->
    {ok, locked, #state{tcp=Tcp}}.

locked({connect, AccountID, LoginIDa, LoginIDb, _Gender}, State) ->
    State#state.tcp ! <<AccountID:32/little>>,

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
        {ok, Account, PacketVer} ->
            GetChars = fun() ->
                           qlc:e(qlc:q([X || X <- mnesia:table(char),
                                             X#char.account_id =:= AccountID]))
                       end,
            {atomic, Chars} = mnesia:transaction(GetChars),

            log:debug("Packet version received.", [{ver, PacketVer}]),

            State#state.tcp ! {packet_handler, char_packets:new(PacketVer)},
            State#state.tcp ! {16#6b, Chars},

            {next_state, valid, State#state{account = Account}};
        invalid ->
            State#state.tcp ! {16#6c, 0},
            {next_state, locked, State}
    end;
locked(Event, State) ->
    log:debug("Character FSM received invalid event.",
              [{event, Event},
               {state, locked}]),
    {next_state, locked, State}.

valid({choose, Num}, State = #state{account = #account{id = AccountID}}) ->
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

            {port, ZonePort} = gen_server:call({zone_master, ZoneNode},
                                               {who_serves, C#char.map}),

            {node, LoginNode} = config:get_env(char, login.node),
            {AccountID, Account, _FSM, SessionIDa, SessionIDb, PacketVer} =
                gen_server_tcp:call({server, LoginNode},
                                    {get_session, AccountID}),

            gen_server_tcp:cast(server,
                                {add_session, {AccountID,
                                               C#char.id,
                                               Account,
                                               C,
                                               self(),
                                               SessionIDa,
                                               SessionIDb,
                                               PacketVer}}),

            State#state.tcp ! {16#71,
                               {C,
                                ZoneIP,
                                ZonePort}},

            {stop, normal, State};
        {atomic, []} ->
            log:warning("Player selected invalid character.",
                        [{account, AccountID}]),
            State#state.tcp ! {16#6c, 1},
            {next_state, valid, State};
        Error ->
            log:warning("Error grabbing selected character.",
                        [{result, Error}]),
            State#state.tcp ! {16#6c, 1},
            {next_state, valid, State}
    end;
valid({create, Name, Str, Agi, Vit, Int, Dex, Luk, Num, HairColour, HairStyle},
      State = #state{account = Account}) ->
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
            State#state.tcp ! {16#6d, Char};
        {atomic, exists} ->
            log:info("Character creation denied (name already in use).",
                     [{account, Account}]),
            State#state.tcp ! {16#6e, 0};
        Error ->
            log:info("Character creation failed.",
                     [{account, Account},
                      {result, Error}]),
            State#state.tcp ! {16#6e, 16#FF}
    end,

    {next_state, valid, State};
valid({delete, CharacterID, EMail}, State = #state{account = #account{id = AccountID, email = AccountEMail}}) ->
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
                    State#state.tcp ! <<16#6f:16/little>>;
                Error ->
                    log:warning("Character deletion failed.",
                                [{char_id, CharacterID},
                                 {account_id, AccountID},
                                 {email, EMail},
                                 {result, Error}]),
                    State#state.tcp ! {16#70, 0}
            end;
        _Invalid ->
            log:warning("Character deletion attempted with wrong e-mail address.",
                        [{email, EMail},
                         {wanted, AccountEMail}]),
            State#state.tcp ! {16#70, 0}
    end,
    {next_state, valid, State};
valid({check_name, AccountID, CharacterID, NewName}, State = #state{account = #account{id = AccountID}}) ->
    Check = fun() ->
                [] = qlc:e(qlc:q([C || C <- mnesia:table(char),
                                       C#char.name =:= NewName])),
                qlc:e(qlc:q([C || C <- mnesia:table(char),
                                  C#char.id =:= CharacterID,
                                  C#char.account_id =:= AccountID]))
            end,

    case mnesia:transaction(Check) of
        {atomic, [Char]} ->
            State#state.tcp ! {16#28e, 1},
            {next_state, renaming, State#state{rename = {Char, NewName}}};
        _Invalid ->
            State#state.tcp ! {16#28e, 0},
            {next_state, valid, State}
    end;
valid(Event, State) ->
    log:debug("Character FSM received invalid event.",
              [{event, Event},
               {state, valid}]),
    {next_state, valid, State}.

renaming({rename, CharacterID},
         State = #state{rename = {Char = #char{id = CharacterID,
                                               renamed = 0},
                                  NewName}}) ->
    Write = fun() ->
                [] = qlc:e(qlc:q([C || C <- mnesia:table(char),
                                       C#char.name =:= NewName])),
                mnesia:write(Char#char{name = NewName, renamed = 1})
            end,

    case mnesia:transaction(Write) of
        {atomic, ok} ->
            State#state.tcp ! {16#290, 0};
        _Error ->
            State#state.tcp ! {16#290, 3}
    end,

    {next_state, valid, State#state{rename = undefined}};
renaming({rename, _CharacterID},
         State = #state{rename = {#char{renamed = 1}, _NewName}}) ->
    State#state.tcp ! {16#290, 1},
    {next_state, valid, State};
renaming(Event, State) ->
    log:debug("Character FSM received invalid event.",
              [{event, Event},
               {state, renaming}]),
    {next_state, renaming, State}.

handle_event(stop, _StateName, StateData) ->
    log:info("Character FSM stopping."),
    {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    log:debug("Character FSM got event.", [{event, Event}, {state, StateName}, {state_data, StateData}]),
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    log:debug("Character FSM got sync event."),
    {next_state, StateName, StateData}.

handle_info({'EXIT', _From, Reason}, _StateName, StateData) ->
    log:debug("Character FSM got EXIT signal.", [{reason, Reason}]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    log:debug("Character FSM got info.", [{info, Info}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #state{account = #account{id = AccountID}}) ->
    log:info("Character FSM waiting for message to keep sessions alive."),

    receive
        keepalive ->
            log:debug("Received keepalive; terminating.", [{account, AccountID}]),
            ok
    after
        300000 ->
            log:debug("No keepalive; removing session.",
                      [{account, AccountID}]),
            gen_server_tcp:cast(server, {remove_session, AccountID}),

            {node, LoginNode} = config:get_env(char, login.node),
            gen_server_tcp:cast({server, LoginNode},
                                {remove_session, AccountID}),

            ok
    end;
terminate(_Reason, _StateName, _StateData) ->
    log:debug("Character FSM terminating."),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
