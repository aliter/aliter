-module(zone_packets_25).

-export([unpack/1, pack/2]).

unpack(X) -> zone_packets_24:unpack(X).

pack(X, Y) -> zone_packets_24:pack(X, Y).
