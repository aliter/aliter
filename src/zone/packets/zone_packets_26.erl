-module(zone_packets_26).

-export([unpack/1, pack/2]).

unpack(<<16#35f:16/little, Position:3/little-binary-unit:8>>) ->
  {walk, decode_position(Position)};

unpack(<<16#360:16/little, Tick:32/little>>) ->
  {tick, Tick};

unpack(<<16#361:16/little, Head:16/little, Body:8>>) ->
  {change_direction, Head, Body};

unpack(<<16#368:16/little, ActorID:32/little>>) ->
  {request_name, ActorID};

unpack(X) -> zone_packets_25:unpack(X).


pack(X, Y) -> zone_packets_25:pack(X, Y).


decode_position(<<XNum, YNum, DNum>>) ->
  X = (XNum bsl 2) bor ((YNum band 16#c0) bsr 6),
  Y = ((YNum band 16#3F) bsl 4) bor ((DNum band 16#f0) bsr 4),
  D = DNum band 16#0F,
  {X, Y, D}.


encode_position(X, Y, D) ->
  A = (X bsr 2) band 16#ff,
  B = ((X bsl 6) bor ((Y bsr 4) band 16#3f)) band 16#ff,
  C = ((Y bsl 4) bor (D band 16#0f)) band 16#ff,
  <<A, B, C>>.


encode_move(X, Y, ToX, ToY) ->
  A = (X bsr 2) band 16#ff,
  B = ((X bsl 6) bor ((Y bsr 4) band 16#3f)) band 16#ff,
  C = ((Y bsl 4) bor ((ToX bsr 6) band 16#0f)) band 16#ff,
  D = ((ToX bsl 2) bor ((ToY bsr 8) band 16#03)) band 16#ff,
  E = ToY band 16#ff,
  F = ((8 bsl 4) bor (8 band 16#0f)) band 16#ff,
  <<A, B, C, D, E, F>>.


pad_to(Bin, Size) ->
  Binary = iolist_to_binary(Bin),
  [ binary:part(Binary, 0, min(byte_size(Binary), Size)),
    binary:copy(<<0>>, Size - byte_size(Binary))
  ].
