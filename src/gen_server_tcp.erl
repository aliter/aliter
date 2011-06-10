-module(gen_server_tcp).

-author('i.am@toogeneric.com').

-behaviour(gen_server).

-include("include/records.hrl").

-export([behaviour_info/1]).

%% API
-export([
    start/3,
    start/4,
    start_link/3,
    start_link/4]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% helpers
-export([
    listener/1,
    client_sup/1]).

% State for the outer manager, wrapping the callback.
-record(state, {module, module_state, supervisor}).


behaviour_info(callbacks) ->
  [ {init, 1},
    {handle_call, 3},
    {handle_cast, 2},
    {handle_info, 2},
    {terminate, 2},
    {code_change, 3}
  ];

behaviour_info(_) -> undefined.


start_link(Name, Module, InitArgs, Options) ->
  gen_server:start_link(
    Name,
    ?MODULE,
    {gen_server_tcp, Module, InitArgs},
    Options
  ).


start_link(Module, InitArgs, Options) ->
  gen_server:start_link(
    ?MODULE,
    {gen_server_tcp, Module, InitArgs},
    Options
  ).

start(Name, Module, InitArgs, Options) ->
  gen_server:start(
    Name,
    ?MODULE,
    {gen_server_tcp, Module, InitArgs},
    Options
  ).


start(Module, InitArgs, Options) ->
  gen_server:start(
    ?MODULE,
    {gen_server_tcp, Module, InitArgs},
    Options
  ).



% gen_server callbacks

init({gen_server_tcp, Module, InitArgs}) ->
  process_flag(trap_exit, true),

  log:debug("Starting generic TCP server.", [{module, Module}]),

  case Module:init(InitArgs) of
    {ok, {Port, FSMModule, PacketHandler}, ModState} ->
      log:debug(
        "TCP server started.",
        [ {module, Module},
          {port, Port},
          {fsm, FSMModule},
          {handler, PacketHandler}
        ]
      ),

      {MState, FArgs} =
        case ModState of
          {X, Y} -> {X, Y};
          X -> {X, []}
        end,

      St = #nb_state{
        port = Port,
        packet_handler = PacketHandler,
        fsm_module = FSMModule,
        fsm_args = FArgs,
        server = self()
      },

      {ok, Sup} = supervisor:start_link(?MODULE, {all, St}),

      { ok,
        #state{
          module = Module,
          module_state = MState,
          supervisor = Sup
        }
      };

    ignore ->
      log:error("TCP server got ignore init result.", [{module, Module}]),
      ignore;

    {stop, Reason} ->
      log:error(
        "TCP server got stop init result.",
        [{module, Module}, {reason, Reason}]
      ),
      {stop, Reason};

    Other ->
      log:error(
        "TCP server got unknown init result.",
        [{module, Module}, {result, Other}]
      ),
      {stop, Other}
  end;

% initialize & supervise both the listener and the FSM
init({all, #nb_state{port = Port, fsm_module = FSMModule} = St}) ->
  { ok,
    { {one_for_one, 2, 60},
      [ { listener(Port),
          { gen_nb_server,
            start_link,
            [ {local, listener(Port)},
              gen_packet_server,
              [St]
            ]
          },
          permanent,
          1000,
          worker,
          []
        },
        { client_sup(Port),
          { supervisor,
            start_link,
            [ {local, client_sup(Port)},
              ?MODULE,
              {clients, FSMModule}
            ]
          },
          permanent,
          infinity,
          supervisor,
          []
        }
      ]
    }
  };

% initialize the FSM
init({clients, FSMModule}) ->
  { ok,
    { {simple_one_for_one, 2, 60},
      [ { undefined,
          {FSMModule, start_link, []},
          temporary,
          301000, % FSMs wait <=5 mins for a signal to keep the login IDs.
          worker,
          [FSMModule]
        }
      ]
    }
  }.


handle_call(Request, From, #state{module = Module, module_state = ModState} = St) ->
  case Module:handle_call(Request, From, ModState) of
    {reply, Reply, NewModState} ->
      {reply, Reply, St#state{module_state = NewModState}};

    {reply, Reply, NewModState, Timeout} ->
      {reply, Reply, St#state{module_state = NewModState}, Timeout};

    {noreply, NewModState} ->
      {noreply, St#state{module_state = NewModState}};

    {noreply, NewModState, Timeout} ->
      {noreply, St#state{module_state = NewModState}, Timeout};

    {stop, Reason, NewModState} ->
      {stop, Reason, St#state{module_state = NewModState}};

    {stop, Reason, Reply, NewModState} ->
      {stop, Reason, Reply, St#state{module_state = NewModState}}
  end.


handle_cast(Request, #state{module = Module, module_state = ModState}=St) ->
  case Module:handle_cast(Request, ModState) of
    {noreply, NewModState} ->
      {noreply, St#state{module_state = NewModState}};

    {noreply, NewModState, Timeout} ->
      {noreply, St#state{module_state = NewModState}, Timeout};

    {stop, Reason, NewModState} ->
      {stop, Reason, St#state{module_state = NewModState}}
  end.


handle_info(Info, #state{module = Module, module_state = ModState}=St) ->
  case Module:handle_info(Info, ModState) of
    {noreply, NewModState} ->
      {noreply, St#state{module_state = NewModState}};

    {noreply, NewModState, Timeout} ->
      {noreply, St#state{module_state = NewModState}, Timeout};

    {stop, Reason, NewModState} ->
      {stop, Reason, St#state{module_state = NewModState}}
  end.


terminate(Reason, #state{module = Module, module_state = ModState, supervisor = Supervisor}) ->
  log:debug("Generic TCP server terminating.",
        [{module, Module},
         {reason, Reason}]),

  %exit(Supervisor, Reason),

  Module:terminate(Reason, ModState).


code_change(OldVsn, #state{module = Module, module_state = ModState}=St, Extra) ->
  case Module:code_change(OldVsn, ModState, Extra) of
    {ok, NewModState} ->
      {ok, St#state{module_state = NewModState}};

    Other -> Other
  end.


% Helpers
listener(Port) ->
  list_to_atom(lists:concat(["listener_", Port])).


client_sup(Port) ->
  list_to_atom(lists:concat(["client_sup_", Port])).
