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
    {ok, {{port, _LoginPort},
          {node, {LoginHost, LoginName}},
          {aliter, LoginAliter},
          Servers}} = application:get_env(aliter, servers),

    {ok, LoginNode} = slave:start_link(LoginHost,
                                       LoginName,
                                       "-pa " ++ LoginAliter ++ "/ebin"),
    rpc:block_call(LoginNode, login, Fun, []),
    slave:stop(LoginNode),

    lists:foreach(fun({_Name, CharConf, _ZoneConf}) ->
                      {node, {CharHost, CharName}} = proplists:lookup(node, CharConf),
                      {aliter, CharAliter} = proplists:lookup(aliter, CharConf),
                      {ok, CharNode} = slave:start_link(CharHost,
                                                        CharName,
                                                        "-pa " ++ CharAliter ++ "/ebin"),
                      rpc:block_call(CharNode, char, Fun, []),
                      slave:stop(CharNode)
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

