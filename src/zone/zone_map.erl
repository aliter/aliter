-module(zone_map).
-behaviour(gen_server).

-include("include/records.hrl").

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(map_state,
        {map,
         players = [],
         npcs = [],
         mobs = []}).


start_link(Map) ->
    log:debug("Zone map server starting.",
              [{map, Map#map.name}]),

    gen_server:start_link({local, list_to_atom("zone_map_" ++ Map#map.name)},
                          ?MODULE,
                          #map_state{map = Map},
                          []).

init(State) ->
    {ok, State}.

handle_call({get_player, Player}, _From, State = #map_state{players = Players}) ->
    {reply, proplists:lookup(Player, Players), State};
handle_call(Request, _From, State) ->
    log:debug("Zone map server got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, State}.

handle_cast({add_player, Player}, State = #map_state{players = Players}) ->
    log:warning("Zone map server adding player.",
                [{player, Player}]),

    {noreply, State#map_state{players = [Player | Players]}};
handle_cast(Cast, State) ->
    log:debug("Zone map server got cast.", [{cast, Cast}]),
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    log:debug("Zone map server got EXIT signal.", [{reason, Reason}]),
    {stop, normal, State};
handle_info(Info, State) ->
    log:debug("Zone map server got info.", [{info, Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    log:info("Zone map server terminating."),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
