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

-define(FEMALE, 0).
-define(MALE, 1).


start_link(Tcp) ->
  gen_fsm:start_link(?MODULE, Tcp, []).


init(Tcp) ->
  {ok, locked, #login_state{tcp = Tcp}}.


locked({login, PacketVer, RawLogin, Password, Region}, State) ->
  log:info("Received login request.",
    [ {packetver, PacketVer},
      {login, RawLogin},
      {password, erlang:md5(Password)},
      {region, Region}]),

  % Create new account if username ends with _M or _F.
  GenderS = string:sub_string(RawLogin, length(RawLogin)-1, length(RawLogin)),
  Login = register_account(RawLogin, Password, GenderS),

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
        {add_session, {A#account.id, self(), LoginIDa, LoginIDb}}),

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

      State#login_state.tcp ! {accept, {LoginIDa,
        LoginIDb,
        A#account.id,
        A#account.gender,
        Servers}},

      State#login_state.tcp ! close,

      valid(stop, State#login_state{
        account = A,
        id_a = LoginIDa,
        id_b = LoginIDb,
        packet_ver = PacketVer});
    [] ->
      L = fun() ->
        qlc:e(qlc:q([X || X <- mnesia:table(account), X#account.login =:= Login]))
      end,

      {atomic, ValidName} = mnesia:transaction(L),

      case ValidName of
        [_A] -> State#login_state.tcp ! {refuse, {1, ""}};
        [] -> State#login_state.tcp ! {refuse, {0, ""}}
      end,

      {next_state, locked, State}
  end.


%% Create account when username ends with _M or _F
register_account(RawLogin, Password, "_M") ->
  create_new_account(RawLogin, Password, ?MALE);

register_account(RawLogin, Password, "_F") ->
  create_new_account(RawLogin, Password, ?FEMALE);

register_account(Login, _Password, _) ->
  Login.

create_new_account(RawLogin, Password, Gender) ->
  Login = string:sub_string(RawLogin, 1, length(RawLogin)-2),

  F = fun() ->
    qlc:e(qlc:q([X || X <- mnesia:table(account),
      X#account.login =:= Login,
      X#account.password =:= erlang:md5(Password)]))
  end,
  {atomic, Verify} = mnesia:transaction(F),

  case Verify of
    [_A] -> log:info("Account already exists.", [{login, Login}]);
    [] ->
      Create = fun() ->
        Account = #account{
          login = Login,
          password = erlang:md5(Password),
          gender = Gender,
          id = mnesia:dirty_update_counter(ids, account, 0)},
        mnesia:write(Account),
        mnesia:dirty_update_counter(ids, account, 1),
        Account
      end,
    mnesia:transaction(Create),
    log:info("Created account", [{login, Login}])
  end,

  Login.


valid(stop, State) ->
  log:debug("Login FSM waiting 5 minutes to exit."),
  {next_state,
   valid,
   State#login_state{die = gen_fsm:send_event_after(5 * 60 * 1000, exit)}};

valid(exit, State) ->
  {stop, normal, State};

valid(Event, State) ->
  ?MODULE:handle_event(Event, chosen, State).


valid(switch_char, _From, State) ->
  gen_fsm:cancel_timer(State#login_state.die),
  {reply, {ok, State}, valid, State}.


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


handle_info({'EXIT', From, Reason}, _StateName, StateData) ->
  log:error("Login FSM got EXIT signal.", [{from, From}, {reason, Reason}]),
  {stop, normal, StateData};

handle_info(Info, StateName, StateData) ->
  log:debug("Login FSM got info.", [{info, Info}]),
  {next_state, StateName, StateData}.


terminate(_Reason, _StateName, #login_state{account = #account{id = AccountID}}) ->
  log:debug("Login FSM terminating.", [{account, AccountID}]),
  gen_server_tcp:cast(server, {remove_session, AccountID});

terminate(_Reason, _StateName, _StateData) ->
  ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.
