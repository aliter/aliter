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
locked(_Event, State) ->
    {next_state, locked, State}.

valid({choose, Num}, State = #state{account = #account{id = AccountID}}) ->
    GetChar = fun() ->
                  qlc:e(qlc:q([X || X <- mnesia:table(char),
                                    X#char.num =:= Num,
                                    X#char.account_id =:= AccountID]))
              end,
    {atomic, [_C]} = mnesia:transaction(GetChar),

    {next_state, valid, State};
% valid({create, Name, Str, Agi, Vit, Int, Dex, Luk, Num, HairColor, HairStyle},
      % State) ->
    % {next_state, valid, State};
valid({delete, _CharacterID, _EMail}, State) ->
    {next_state, valid, State};
valid(_Event, State) ->
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
