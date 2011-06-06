-module(login_srv).
-behaviour(gen_server_tcp).

-include("include/records.hrl").

-export([start_link/1]).

-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).


start_link(Conf) ->
  config:set_env(login, Conf),

  {port, Port} = config:get_env(login, server.port),

  log:info("Starting login server.", [{port, Port}]),

  application:set_env(mnesia, dir, config:db()),

  ok = mnesia:start(),

  ok = mnesia:wait_for_tables([account], 2000),

  gen_server_tcp:start_link({local, server}, ?MODULE, Port, []).


init(Port) ->
  {ok, {Port, login_fsm, login_packets}, []}.


handle_call({verify_session, AccountID, LoginIDa, LoginIDb}, _From, Sessions) ->
  log:debug("Verifying session.",
    [{account, AccountID}, {ids, {LoginIDa, LoginIDb}}, {session, Sessions}]),

  case proplists:lookup(AccountID, Sessions) of
    {AccountID, FSM, LoginIDa, LoginIDb} ->
      {reply, {ok, FSM}, Sessions};
    _ ->
      {reply, invalid, Sessions}
  end;

handle_call({get_session, AccountID}, _From, Sessions) ->
  {reply, proplists:lookup(AccountID, Sessions), Sessions};

handle_call(Request, _From, Sessions) ->
  log:debug("Login server got call.", [{call, Request}]),
  {reply, {illegal_request, Request}, Sessions}.


handle_cast({#apirequest{add_account = A}, From}, Sessions) when A =/= undefined ->
  log:debug("Login server got API request",
    [{request, log:yellow(add_account)}, {account, A}]),

  Create = fun() ->
     Account = A#account{id = mnesia:dirty_update_counter(ids, account, 0)},
     mnesia:write(Account),
     mnesia:dirty_update_counter(ids, account, 1),
     Account
  end,
  case mnesia:transaction(Create) of
    {atomic, Account} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
        account = Account});
    {aborted, _Reason} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
        info = "Account could not be created."})
  end,

  {noreply, Sessions};

handle_cast({#apirequest{get_account = A}, From}, Sessions) when A =/= undefined ->
  log:debug("Login server got API request",
    [{request, log:yellow(get_account)}, {account, A}]),

  Get = fun() -> mnesia:match_object(api:dc(A)) end,
  case mnesia:transaction(Get) of
    {atomic, Accounts} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
        account = Accounts});
    {aborted, _Reason} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
        info = "No accounts found."})
  end,

  {noreply, Sessions};

handle_cast({#apirequest{delete_account = A}, From}, Sessions) when A =/= undefined ->
  log:debug("Login server got API request",
    [{request, log:yellow(delete_account)}, {account, A}]),

  Delete = fun() ->
    Accounts = mnesia:match_object(api:dc(A)),
    lists:foreach(fun mnesia:delete_object/1, Accounts),
    Accounts
  end,
  case mnesia:transaction(Delete) of
    {atomic, Accounts} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
        info = "Accounts deleted.",
        account = Accounts});
    {aborted, _Reason} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
        info = "Account not found."})
  end,

  {noreply, Sessions};

handle_cast({#apirequest{update_account = A}, From}, Sessions) when A =/= undefined ->
  log:debug("Login server got API request",
    [{request, log:yellow(update_account)}, {account, A}]),

  Update = fun() -> mnesia:write(A) end,
  case mnesia:transaction(Update) of
    {atomic, ok} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
        account = A});
    {aborted, _Reason} ->
      From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
        info = "Account could not be updated."})
  end,

  {noreply, Sessions};

handle_cast({add_session, Session}, Sessions) ->
  log:debug("Login server adding session.", [{session, Session}]),
  {noreply, [Session | Sessions]};

handle_cast({remove_session, AccountID}, Sessions) ->
  log:debug("Login server removing session.", [{account, AccountID}]),
  {noreply, lists:keydelete(AccountID, 1, Sessions)};

handle_cast(Cast, Sessions) ->
  log:debug("Login server got cast.", [{cast, Cast}]),
  {noreply, Sessions}.

handle_info({'EXIT', From, Reason}, Sessions) ->
  log:error("Login server got EXIT signal.", [{from, From}, {reason, Reason}]),
  {stop, normal, Sessions};

handle_info(Info, Sessions) ->
  log:debug("Login server got info.", [{info, Info}]),
  {noreply, Sessions}.


terminate(_Reason, _Sessions) ->
  log:info("Login server terminating.", [{node, node()}]),
  mnesia:stop(),
  ok.


code_change(_OldVsn, Sessions, _Extra) ->
  {ok, Sessions}.

