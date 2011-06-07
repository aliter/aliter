-module(login_fsm).
-behaviour(gen_fsm).

-include("include/records.hrl").

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


start_link(TCP) ->
  gen_fsm:start_link(?MODULE, TCP, []).


init(TCP) ->
  {ok, DB} = erldis:connect(), % TODO: config
  {ok, locked, #login_state{tcp = TCP, db = DB}}.


locked(
    {login, PacketVer, RawLogin, Password, Region},
    State = #login_state{tcp = TCP, db = DB}) ->
  log:info(
    "Received login request.",
    [ {packetver, PacketVer},
      {login, RawLogin},
      {password, erlang:md5(Password)},
      {region, Region}
    ]
  ),

  % Create new account if username ends with _M or _F.
  GenderS = string:sub_string(RawLogin, length(RawLogin)-1, length(RawLogin)),
  Login = register_account(DB, RawLogin, Password, GenderS),

  Versioned = State#login_state{packet_ver = PacketVer},

  log:info("Pre-auth.", [{login, Login}]),

  case Login of
    A = #account{} ->
      successful_login(A, Versioned);

    _ ->
    GetID = erldis:get(DB, ["account:", Login]),
    case GetID of
      % Bad login
      nil ->
        TCP ! {refuse, {0, ""}},
        {next_state, locked, State};

      ID ->
        GetPassword = erldis:hget(DB, ["account:", ID], "password"),

        Hashed = erlang:md5(Password),

        if
          % Bad password
          GetPassword /= Hashed ->
            TCP ! {refuse, {1, ""}},
            {next_state, locked, State};

          % Successful auth
          true ->
            successful_login(db:get_account(DB, ID), Versioned)
        end
    end
  end.


successful_login(A, State) ->
  % Generate random IDs using current time as initial seed
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),

  {LoginIDa, LoginIDb} =
    {random:uniform(16#FFFFFFFF), random:uniform(16#FFFFFFFF)},

  gen_server_tcp:cast(
    server,
    {add_session, {A#account.id, self(), LoginIDa, LoginIDb}}
  ),

  {chars, CharServers} = config:get_env(login, chars),
  Servers =
    lists:map(
      fun({_Node, Conf}) ->
        {name, Name} = config:find(server.name, Conf),

        {ip, IP} = config:find(server.ip, Conf),
        {port, Port} = config:find(server.port, Conf),

        {maintenance, Maintenance} =
          config:find(server.maintenance, Conf),

        {new, New} = config:find(server.new, Conf),

        {IP, Port, Name, 0, Maintenance, New}
      end,

      CharServers
    ),

  State#login_state.tcp !
    { accept,
      { LoginIDa,
        LoginIDb,
        A#account.id,
        A#account.gender,
        Servers
      }
    },

  State#login_state.tcp ! close,

  valid(
    stop,
    State#login_state{
      account = A,
      id_a = LoginIDa,
      id_b = LoginIDb
    }
  ).


%% Create account when username ends with _M or _F
register_account(C, RawLogin, Password, "_M") ->
  create_new_account(C, RawLogin, Password, ?MALE);

register_account(C, RawLogin, Password, "_F") ->
  create_new_account(C, RawLogin, Password, ?FEMALE);

register_account(_, Login, _Password, _) ->
  Login.


create_new_account(C, RawLogin, Password, Gender) ->
  Login = string:sub_string(RawLogin, 1, length(RawLogin)-2),

  Check = erldis:get(C, ["account:", Login]),

  log:info("Check.", [{check, Check}]),

  case Check of
    nil ->
      log:info("Created account", [{login, Login}]),

      db:save_account(
        C,
        #account{
          login = Login,
          password = erlang:md5(Password),
          gender = Gender
        }
      );

    _ ->
      log:info("Account already exists; ignore.", [{login, Login}]),
      Login
  end.


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
