-module(zone_packets_25).

-export([unpack/1, pack/2]).

unpack(<<16#437:16/little, TargetID:32/little, Action:8>>) ->
  {action_request, TargetID, Action};

%unpack(<<16#44a:16/little, _Unknown:32/little>>) ->
  % TODO
  %unknown;

unpack(X) -> zone_packets_24:unpack(X).

pack(X, Y) -> zone_packets_24:pack(X, Y).
