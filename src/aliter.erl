-module(aliter).
-behaviour(application).

-include("include/records.hrl").

-export([start/2,
         shutdown/0,
         stop/1,
         install/0,
         uninstall/0,
         reinstall/0]).

start(_Type, StartArgs) ->
    aliter_config:load(main),

    case aliter_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

shutdown() ->
    application:stop(aliter).

stop(_State) ->
    ok.

callAll(Fun) ->
    aliter_config:load(main),
    {ok, {LoginConf, Servers}} = application:get_env(aliter, servers),

    {aliter, LoginAliter} = proplists:lookup(aliter, LoginConf),
    {node, {LoginHost, LoginName}} = proplists:lookup(node, LoginConf),

    case slave:start_link(LoginHost,
                          LoginName,
                          "-pa " ++ LoginAliter ++ "/ebin") of
        {ok, LoginNode} ->
            LoginRes = rpc:block_call(LoginNode, login, Fun, []),
            error_logger:info_report([login_install_report,
                                      {result, LoginRes}]),
            slave:stop(LoginNode);
        {error, LoginReason} ->
            error_logger:warning_report([login_install_canceled, {reason, LoginReason}])
    end,

    lists:foreach(fun({Name, CharConf, ZoneConf}) ->
                      {node, {CharHost, CharName}} = proplists:lookup(node, CharConf),
                      {aliter, CharAliter} = proplists:lookup(aliter, CharConf),
                      case slave:start_link(CharHost,
                                            CharName,
                                            "-pa " ++ CharAliter ++ "/ebin") of
                          {ok, CharNode} ->
                              CharRes = rpc:block_call(CharNode, char, Fun, []),
                              error_logger:info_report([char_install_report,
                                                        {name, Name},
                                                        {result, CharRes}]),
                              slave:stop(CharNode);
                          {error, CharReason} ->
                              error_logger:warning_report([char_install_canceled,
                                                           {name, Name},
                                                           {reason, CharReason}])
                      end,


                      {node, {ZoneHost, ZoneName}} = proplists:lookup(node, ZoneConf),
                      {aliter, ZoneAliter} = proplists:lookup(aliter, ZoneConf),
                      case slave:start_link(ZoneHost,
                                            ZoneName,
                                            "-pa " ++ ZoneAliter ++ "/ebin") of
                          {ok, ZoneNode} ->
                              ZoneRes = rpc:block_call(ZoneNode, zone, Fun, []),
                              error_logger:info_report([zone_install_report,
                                                        {name, Name},
                                                        {result, ZoneRes}]),
                              slave:stop(ZoneNode);
                          {error, ZoneReason} ->
                              error_logger:warning_report([zone_install_canceled,
                                                           {name, Name},
                                                           {reason, ZoneReason}])
                      end
                      % zone:install(Name)
                  end,
                  Servers).

install() ->
    callAll(install).

uninstall() ->
    callAll(uninstall).

reinstall() ->
    uninstall(),
    install().

