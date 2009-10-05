-module(aliter_sup).
-behaviour(supervisor).

-export([start_link/1,
         init/1]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init([]) ->
    {ok, {LoginConf, _Servers}} = application:get_env(aliter, servers),

    {node, {LoginHost, LoginName}} = proplists:lookup(node, LoginConf),
    {aliter, LoginAliter} = proplists:lookup(aliter, LoginConf),

    {ok, LoginNode} = slave:start_link(LoginHost,
                                       LoginName,
                                       "-pa " ++ LoginAliter ++ "/ebin"),

    {ok, {{one_for_one, 2, 60},
          [{login,
            {rpc, block_call, [LoginNode, login, start, [normal, []]]},
            permanent,
            1000,
            worker,
            []},
           {char, {char, start, [normal, []]}, permanent, 1000, worker, []},
           {api, {api, start_link, []}, permanent, 1000, worker, []}]}}.
