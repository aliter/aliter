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

-record(state, {tcp,
                account,
                character}).

start_link(Tcp) ->
    gen_fsm:start_link(?MODULE, Tcp, []).

init(Tcp) ->
    {ok, locked, #state{tcp=Tcp}}.

locked({connect, AccountID, CharacterID, SessionIDa, Gender}, State) ->
    {char, CharNode} = config:get_env(zone, server.char),
    Session = gen_server_tcp:call({server, CharNode},
                                  {verify_session, AccountID, CharacterID, SessionIDa}),

    case Session of
        {ok, A, C, PacketVer} ->
            State#state.tcp ! {packet_handler, zone_packets:new(PacketVer)},

            State#state.tcp ! {16#283, SessionIDa},
            State#state.tcp ! {16#73, {zone_master:tick(), {53, 111, 0}}},
            State#state.tcp ! {16#8e, "Welcome to Aliter!"},

            {next_state, valid, State#state{account = A, character = C}};
        invalid ->
            log:warning("Invalid zone login attempt caught.",
                        [{account_id, AccountID},
                         {character_id, CharacterID}]),
            {next_state, locked, State}
    end;
locked(Event, State) ->
    log:debug("Zone FSM received invalid event.",
              [{event, Event},
               {state, locked}]),
    {next_state, locked, State}.

valid(Event, State) ->
    log:debug("Zone FSM received invalid event.",
              [{event, Event},
               {state, valid}]),
    {next_state, valid, State}.

handle_event(stop, _StateName, StateData) ->
    log:info("Zone FSM stopping."),
    {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    log:debug("Zone FSM got event.", [{event, Event}, {state, StateName}, {state_data, StateData}]),
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    log:debug("Zone FSM got sync event."),
    {next_state, StateName, StateData}.

handle_info({'EXIT', _From, Reason}, _StateName, StateData) ->
    log:debug("Zone FSM got EXIT signal.", [{reason, Reason}]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    log:debug("Zone FSM got info.", [{info, Info}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    log:info("Zone FSM terminating."),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
