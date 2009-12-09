-module(nif).
-export([init/0, pathfind/3]).

init() ->
    ok = erlang:load_nif("priv/nif", 0),
    true.

pathfind(_, _, _) ->
    log:error("NIF pathfinder not loaded.").
