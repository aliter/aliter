-module(aliter).
-behaviour(application).

-include("include/records.hrl").

-export([
  start/2,
  shutdown/0,
  stop/1,
  path/1,
  install/0,
  uninstall/0,
  reinstall/0]).


start(_Type, StartArgs) ->
  aliter_sup:start_link(StartArgs).


shutdown() ->
  application:stop(aliter).


stop(_State) ->
  ok.


path(A) ->
  "-pa " ++ A ++ "/ebin -pa " ++ A ++ "/scbin" ++
    " -pa " ++ A ++ "/lib/erldis/ebin" ++
    " -pa " ++ A ++ "/lib/erldis/deps/gen_server2/ebin" ++
    " -pa " ++ A ++ "/lib/elixir/ebin -pa " ++ A ++ "/lib/elixir/exbin".


call_all(Fun) ->
  {Login, Char, Zone} = config:load(),

  {host, {LoginHost, LoginName}} = config:find(server.host, Login),
  {aliter, LoginAliter} = config:find(server.aliter, Login),

  case slave:start_link(LoginHost, LoginName, path(LoginAliter)) of
    {ok, LoginNode} ->
      LoginRes = rpc:block_call(LoginNode, login, Fun, []),
      error_logger:info_report(
        [login_install_report, {result, LoginRes}]
      ),
      slave:stop(LoginNode);

    { error, LoginReason} ->
      error_logger:warning_report(
        [login_install_canceled, {reason, LoginReason}]
      )
  end,

  lists:foreach(
    fun({Node, Conf}) ->
      {host, {Host, Name}} = config:find(server.host, Conf),
      {aliter, Aliter} = config:find(server.aliter, Conf),
      case slave:start_link(
        Host, Name, path(Aliter)) of {ok, Node} ->
          CharRes = rpc:block_call(Node, char, Fun, []),
          error_logger:info_report(
            [char_install_report, {name, Name}, {result, CharRes}]
          ),
          slave:stop(Node);

        { error, CharReason} ->
          error_logger:warning_report(
            [char_install_canceled, {name, Name}, {reason, CharReason}]
          )
      end
    end,

    Char
  ),

  lists:foreach(
    fun({Node, Conf}) ->
      {host, {Host, Name}} = config:find(server.host, Conf),
      {aliter, Aliter} = config:find(server.aliter, Conf),
      case slave:start_link(
        Host, Name, path(Aliter)) of {ok, Node} ->
          ZoneRes = rpc:block_call(Node, zone, Fun, []),
          error_logger:info_report(
            [zone_install_report, {name, Name}, {result, ZoneRes}]
          ),
          slave:stop(Node);

        { error, ZoneReason} ->
          error_logger:warning_report(
            [zone_install_canceled, {name, Name}, {reason, ZoneReason}]
          )
      end
    end,

    Zone
  ).


install() ->
  call_all(install).


uninstall() ->
  call_all(uninstall).


reinstall() ->
  uninstall(),
  install().

