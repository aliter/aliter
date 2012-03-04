-module(char_packets_25).

-export([unpack/1, pack/2]).

unpack(X) -> char_packets_24:unpack(X).

pack(X, Y) -> char_packets_24:pack(X, Y).
