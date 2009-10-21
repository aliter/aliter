-module(packets, [PacketVer]).

-export([packet_size/1]).


mod_for(Version) ->
    list_to_atom(lists:concat(["packets_", Version])).

packet_size(Header) ->
    (mod_for(PacketVer)):packet_size(Header).

