-module(gen_packet_server).

-author('i.am@toogeneric.com').

-behaviour(gen_nb_server).

-include("include/records.hrl").

-export([
    init/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    sock_opts/0,
    new_connection/4]).

-export([
    client_worker/3,
    parse_loop/3]).


init([St = #nb_state{port = Port}], State) ->
  gen_nb_server:add_listen_socket(
    {"0.0.0.0", Port},
    gen_nb_server:store_cb_state(St, State)
  ).


handle_call(Msg, _From, State) ->
  log:warning("Got unknown call.", [{msg, Msg}]),
  {reply, ignored, State}.


handle_cast(Msg, State) ->
  log:warning("Got unknown cast.", [{msg, Msg}]),
  {noreply, State}.


handle_info(Msg, State) ->
  log:warning("Got unknown info.", [{msg, Msg}]),
  {noreply, State}.


terminate(Reason, _State) ->
  log:warning("Packet server terminating.", [{reason, Reason}]),
  ok.


sock_opts() ->
  [binary, {packet, raw}, {active, false}].


new_connection(_ClientIP, _ClientPort, Sock, State) ->
  #nb_state{
    port = Port, 
    packet_handler = PacketHandler,
    fsm_args = FArgs,
    server = Server
  } = gen_nb_server:get_cb_state(State),

  log:info(
    "Client connected.",
    [{client, element(2, inet:peername(Sock))}]
  ),

  Pid = spawn(fun() ->
    Args =
      case FArgs of
        [] -> [self()];
        _ -> [{self(), FArgs}]
      end,

    {ok, FSM} = supervisor:start_child(gen_server_tcp:client_sup(Port), Args),
    gen_fsm:send_all_state_event(FSM, {set_server, Server}),
    client_worker(Sock, FSM, PacketHandler)
  end),

  gen_tcp:controlling_process(Sock, Pid),

  ok = inet:setopts(Sock, [{active, once}]),

  {ok, State}.


%% @hidden Handle a client's TCP connection.
client_worker(Socket, FSM, PacketHandler) ->
  receive
    {tcp, Socket, Packet} ->
      log:debug("Received packet.", [{packet, Packet}]),

      Event = PacketHandler:unpack(Packet),
      gen_fsm:send_event(FSM, Event),

      ?MODULE:client_worker(Socket, FSM, PacketHandler);

    {tcp_closed, Socket} ->
      log:info("Client disconnected."),
      gen_fsm:send_event(FSM, stop);

    {parse, PacketHandler} ->
      log:debug("Changing to parse mode with packet handler.",
            [{handler, PacketHandler}]),

      Loop = self(),
      Parser =
        spawn(
          ?MODULE,
          parse_loop,
          [Socket, PacketHandler, Loop]
        ),

      gen_tcp:controlling_process(Socket, Parser),

      ?MODULE:client_worker(Socket, FSM, PacketHandler);

    {send_packets, Packets} ->
      log:warning("Sending multiple packets.", [{packets, Packets}]),

      Binaries = lists:map(
        fun(Packet) ->
          case verify(Packet, PacketHandler) of
            {ok, Binary} -> Binary
          end
        end, Packets),

      gen_tcp:send(Socket, iolist_to_binary(Binaries)),
      ?MODULE:client_worker(Socket, FSM, PacketHandler);

    {Packet, Data} ->
      Packed = iolist_to_binary(PacketHandler:pack(Packet, Data)),

      case verify({Packet, Data}, PacketHandler) of
        {ok, Binary} ->
          log:debug("Sending data.", [{data, Data}, {packet, Packed}]),
          gen_tcp:send(Socket, Binary);

        {badsize, Wanted} ->
          log:error("Ignored attempt to send packet of invalid length.",
            [ {packet, Packet},
              {data, Data},
              {wanted, Wanted},
              {got, byte_size(Packed)}])
      end,

      ?MODULE:client_worker(Socket, FSM, PacketHandler);

    Packet when is_binary(Packet) ->
      log:debug("Sending raw packet.", [{packet, Packet}]),

      gen_tcp:send(Socket, Packet),
      ?MODULE:client_worker(Socket, FSM, PacketHandler);

    close ->
      log:debug("Closing TCP server from call."),
      gen_tcp:close(Socket);

    Other ->
      log:warning("Generic TCP server got unknown data.", [{data, Other}])
  end.


%% @hidden Pack and verify a packet size.
verify({Packet, Data}, PacketHandler) ->
  Packed = iolist_to_binary(PacketHandler:pack(Packet, Data)),

  <<Header:16/little, _/binary>> = Packed,
  Size = PacketHandler:packet_size(Header),

  if
    Size == 0;
    byte_size(Packed) == Size ->
      {ok, Packed};

    true ->
      {badsize, Size}
  end.


%% @hidden Receive a packet, skipping unknown ones.
parse_loop(Socket, PacketHandler, Loop) ->
  case gen_tcp:recv(Socket, 1) of
    {ok, <<H1>>} ->
      case gen_tcp:recv(Socket, 1, 0) of
        {ok, <<H2>>} ->
          <<Header:16/little>> = <<H1, H2>>,

          case PacketHandler:packet_size(Header) of
            undefined ->
              log:debug("Received unknown packet.", [{header, Header}]),
              ?MODULE:parse_loop(Socket, PacketHandler, Loop);

            % Variable-length packet
            0 ->
              {ok, <<Length:16/little>>} = gen_tcp:recv(Socket, 2),
              {ok, Rest} = gen_tcp:recv(Socket, Length - 4),

              Loop !
                { tcp,
                  Socket,
                  <<Header:16/little,
                    Length:16/little,
                    Rest/binary>>
                },

              ?MODULE:parse_loop(Socket, PacketHandler, Loop);

            % Already read all of it!
            2 ->
              Loop ! {tcp, Socket, <<Header:16/little>>},
              ?MODULE:parse_loop(Socket, PacketHandler, Loop);

            Size ->
              {ok, Rest} = gen_tcp:recv(Socket, Size - 2),
              Loop ! {tcp, Socket, <<Header:16/little, Rest/binary>>},
              ?MODULE:parse_loop(Socket, PacketHandler, Loop)
          end;

        {error, timeout} ->
          log:debug("Ignoring rest."),
          ?MODULE:parse_loop(Socket, PacketHandler, Loop);

        {error, closed} ->
          Loop ! {tcp_closed, Socket}
      end;

    {error, closed} ->
      Loop ! {tcp_closed, Socket}
  end.
