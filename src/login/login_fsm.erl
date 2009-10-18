-module(login_fsm).
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
         valid/2,
         valid/3]).


start_link(Tcp) ->
    gen_fsm:start_link(?MODULE, Tcp, []).

init(Tcp) ->
    {ok, locked, #login_state{tcp = Tcp}}.


locked({login, PacketVer, Login, Password, Region}, State) ->
    log:info("Received login request.",
             [{packetver, PacketVer},
              {login, Login},
              {password, erlang:md5(Password)},
              {region, Region}]),

    F = fun() ->
           qlc:e(qlc:q([X || X <- mnesia:table(account),
                             X#account.login =:= Login,
                             X#account.password =:= erlang:md5(Password)]))
        end,

    {atomic, Verify} = mnesia:transaction(F),

    case Verify of
        [A] ->
            % Generate random IDs using current time as initial seed
            {A1, A2, A3} = now(),
            random:seed(A1, A2, A3),

            {LoginIDa, LoginIDb} = {random:uniform(16#FFFFFFFF),
                                    random:uniform(16#FFFFFFFF)},

            gen_server_tcp:cast(server,
                                {add_session, {A#account.id,
                                               self(),
                                               LoginIDa,
                                               LoginIDb}}),

            {chars, CharServers} = config:get_env(login, chars),
            Servers = lists:map(fun({_Node, Conf}) ->
                                    {name, Name} = config:find(server.name, Conf),
                                    {ip, IP} = config:find(server.ip, Conf),
                                    {port, Port} = config:find(server.port, Conf),
                                    {maintenance, Maintenance} = config:find(server.maintenance, Conf),
                                    {new, New} = config:find(server.new, Conf),

                                    {IP, Port, Name, 0, Maintenance, New}
                                end,
                                CharServers),

            State#login_state.tcp ! {16#69, {LoginIDa,
                                       LoginIDb,
                                       A#account.id,
                                       Servers}},

            {next_state, valid, State#login_state{account = A,
                                                  id_a = LoginIDa,
                                                  id_b = LoginIDb,
                                                  packet_ver = PacketVer}};
        [] ->
            L = fun() ->
                    qlc:e(qlc:q([X || X <- mnesia:table(account),
                                      X#account.login =:= Login]))
                end,

            {atomic, ValidName} = mnesia:transaction(L),

            case ValidName of
                [_A] -> State#login_state.tcp ! {16#6a, {1, ""}};
                [] -> State#login_state.tcp ! {16#6a, {0, ""}}
            end,

            {next_state, locked, State}
    end.

valid(stop, State) ->
    gen_fsm:send_event_after(60 * 5 * 1000, exit),
    {next_state, valid, State};
valid(exit, State) ->
    {stop, normal, State};
valid(Event, State) ->
    ?MODULE:handle_event(Event, chosen, State).

valid(switch_char, _From, State) ->
    {stop, normal, {ok, State}, State}.

handle_event(stop, _StateName, StateData) ->
    log:info("Login FSM stopping."),
    {stop, normal, StateData};
handle_event({set_server, Server}, StateName, StateData) ->
    {next_state, StateName, StateData#login_state{server = Server}};
handle_event(Event, StateName, StateData) ->
    log:debug("Login FSM got event.", [{even, Event}, {state, StateName}, {state_data, StateData}]),
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    log:debug("Login FSM got sync event."),
    {next_state, StateName, StateData}.

handle_info({'EXIT', _From, Reason}, StateName, StateData) ->
    log:debug("Login FSM got EXIT signal.", [{reason, Reason}]),

    % Wait 5 minutes, then kill the FSM
    gen_fsm:send_event_after(60 * 5 * 1000, stop),

    {next_state, StateName, StateData};
handle_info(Info, StateName, StateData) ->
    log:debug("Login FSM got info.", [{info, Info}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #login_state{account = #account{id = AccountID}}) ->
    log:debug("Login FSM terminating.",
              [{account, AccountID}]),

    gen_server_tcp:cast(server, {remove_session, AccountID});
terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
