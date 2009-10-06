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

-export([init/1, locked/2, valid/2]).

-record(state, {tcp,
                account}).

start_link(Tcp) ->
    gen_fsm:start_link(?MODULE, Tcp, []).

init(Tcp) ->
    {ok, locked, #state{tcp=Tcp}}.

locked({connect, AccountID, LoginIDa, LoginIDb, _Gender}, State) ->
    {ok, LoginNode} = application:get_env(char, login_node),

    Verify = gen_server_tcp:call({listener, LoginNode},
                                 {verify_session,
                                  AccountID,
                                  LoginIDa,
                                  LoginIDb}),

    State#state.tcp ! <<AccountID:32/little>>,

    log:info("Character connect request.", [{account, AccountID}, {ids, {LoginIDa, LoginIDb}}, {verified, Verify}]),

    case Verify of
        {ok, Account} ->
            GetChars = fun() ->
                           qlc:e(qlc:q([X || X <- mnesia:table(char),
                                             X#char.account_id =:= AccountID]))
                       end,
            {atomic, Chars} = mnesia:transaction(GetChars),

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
            {ok, ZoneConf} = application:get_env(char, zone_conf),

            State#state.tcp ! {16#71,
                               C#char.id,
                               (C#char.map) ++ ".gat",
                               proplists:get_value(host, ZoneConf),
                               proplists:get_value(port, ZoneConf)},

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
                         mnesia:delete(char, CharacterID),
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
                    State#state.tcp ! {16#70, 1}
            end;
        _Invalid ->
            log:warning("Character deletion attempted with wrong e-mail address.",
                        [{email, EMail},
                         {wanted, AccountEMail}]),
            State#state.tcp ! {16#70, 0}
    end,
    {next_state, valid, State};
valid(Event, State) ->
    log:debug("Character FSM received invalid event.",
              [{event, Event},
               {state, valid}]),
    {next_state, valid, State}.

handle_event(stop, _StateName, StateData) ->
    log:info("Character FSM stopping."),
    {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    log:debug("Character FSM got event.", [{event, Event}, {state, StateName}, {state_data, StateData}]),
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    log:debug("Character FSM got sync event."),
    {next_state, StateName, StateData}.

handle_info(Info, StateName, StateData) ->
    log:debug("Character FSM got info.", [{info, Info}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    log:info("Character FSM terminating."),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
