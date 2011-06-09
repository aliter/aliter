-module(zone_master).
-behaviour(gen_server).

-include("include/records.hrl").

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([
    tick/0,
    server_for/1]).

-record(state, {npc_id}).


start_link(Conf) ->
  config:set_env(zone, Conf),

  application:set_env(zone, started, now()),

  log:debug("Starting master zone server."),

  gen_server:start_link({local, ?MODULE}, ?MODULE, {server, Conf}, []).


init({server, Conf}) ->
  supervisor:start_link({local, zone_master_sup}, ?MODULE, supervisor),

  application:set_env(zone, tick, 0),

  ok = elixir:start_app(),

  AllMaps = maps:read_cache("priv/maps"),

  nif:init(),

  {zones, Zones} = config:find(server.zones, Conf),
  lists:foreach(
    fun({Port, ZoneMaps}) ->
      Names = [list_to_binary(X) || X <- ZoneMaps],

      log:debug("Starting slave.", [{port, Port}, {maps, Names}]),

      Maps =
        lists:filter(
          fun(M) ->
            lists:member(M#map.name, Names)
          end,
          AllMaps
        ),

      supervisor:start_child(zone_master_sup, [Port, Maps])
    end,
    Zones
  ),

  zone_npc:load_all(),

  {ok, #state{npc_id = 5000000}};


init(supervisor) ->
  { ok,
    { {simple_one_for_one, 2, 60},
      [ { undefined,
          {zone_srv, start_link, []},
          permanent,
          infinity,
          supervisor,
          []
        }
      ]
    }
  }.


handle_call({who_serves, Map}, _From, State) ->
  log:debug("Zone master server got who_serves call.", [{map, Map}]),
  { reply,
    who_serves(Map, supervisor:which_children(zone_master_sup)),
    State
  };

handle_call({get_player, ActorID}, _From, State) ->
  log:debug("Zone master server got get_player call.", [{actor, ActorID}]),

  { reply,
    get_player(
      ActorID,
      supervisor:which_children(zone_master_sup)
    ),
    State
  };

handle_call({get_player_by, Pred}, _From, State) ->
  log:debug("Zone master got get_player_by call."),
  { reply,
    get_player_by(
      Pred,
      supervisor:which_children(zone_master_sup)
    ),
    State
  };

handle_call(player_count, _from, State) ->
  { reply,
    player_count(supervisor:which_children(zone_master_sup)),
    State
  };

handle_call(Request, _From, State) ->
  log:debug("Zone master server got call.", [{call, Request}]),
  {reply, {illegal_request, Request}, State}.


handle_cast({send_to_all, Msg}, State) ->
  lists:foreach(
    fun({_ID, Server, _Type, _Modules}) ->
      gen_server:cast(Server, Msg)
    end,
    supervisor:which_children(zone_master_sup)
  ),

  {noreply, State};

handle_cast(
    {register_npc, Name, Sprite, Map, {X, Y}, Direction, Object},
    State = #state{npc_id = Id}) ->
  lists:foreach(
    fun({_ID, Server, _Type, _Modules}) ->
      gen_server:cast(
        Server,
        { register_npc,
          #npc{
            id = Id,
            name = Name,
            sprite = Sprite,
            map = Map,
            coordinates = {X, Y},
            direction = Direction,
            main = Object
          }
        })
    end,
    supervisor:which_children(zone_master_sup)
  ),

  {noreply, State#state{npc_id = Id + 1}};

handle_cast(Cast, State) ->
  log:debug("Zone master server got cast.", [{cast, Cast}]),
  {noreply, State}.


handle_info({'EXIT', From, Reason}, State) ->
  log:error("Zone master got EXIT signal.", [{from, From}, {reason, Reason}]),
  {stop, normal, State};

handle_info(Info, State) ->
  log:debug("Zone master server got info.", [{info, Info}]),
  {noreply, State}.


terminate(_Reason, _State) ->
  log:info("Zone master server terminating."),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


who_serves(_Map, []) ->
  none;

who_serves(Map, [{_Id, Server, _Type, _Modules} | Servers]) ->
  case gen_server:call(Server, {provides, Map}) of
    {yes, Port} ->
      {zone, Port, Server};

    no ->
      who_serves(Map, Servers)
  end.


player_count([]) ->
  0;

player_count([{_Id, Server, _Type, _Modules} | Servers]) ->
  gen_server:call(Server, player_count) +
    player_count(Servers).


get_player(_ActorID, []) ->
  none;

get_player(ActorID, [{_Id, Server, _Type, _Modules} | Servers]) ->
  case gen_server:call(Server, {get_player, ActorID}) of
    {ok, FSM} ->
      {ok, FSM};

    none ->
      get_player(ActorID, Servers)
  end.


get_player_by(_Pred, []) ->
  none;

get_player_by(Pred, [{_Id, Server, _Type, _Modules} | Servers]) ->
  log:debug(
    "Looking for player from zone_master.",
    [{server, Server}, {pred, Pred}]
  ),

  case gen_server:call(Server, {get_player_by, Pred}) of
    {ok, State} ->
      {ok, State};

    none ->
      get_player_by(Pred, Servers)
  end.


tick() ->
  {ok, Started} = application:get_env(zone, started),
  round(timer:now_diff(now(), Started) / 1000).


server_for(Port) ->
  list_to_atom(lists:concat(["zone_server_", Port])).
