-module(aliter_sup).
-behaviour(supervisor).

-export([start_link/1,
         init/1]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init([]) ->
    {ok, {{port, _LoginPort},
          {node, {LoginHost, LoginName}},
          {aliter, LoginAliter},
          _CharZones}} = application:get_env(aliter, servers),

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
           % {zone_srv, {zone_srv, start_link, []}, permanent, 1000, worker, [zone_srv]}]}}.
