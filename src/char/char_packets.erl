-module(char_packets, [Version]).

-export([unpack/1, pack/2]).

-define(VER, (mod_for(Version))).

mod_for(Version) ->
    list_to_atom(lists:concat(["char_packets_", Version])).

unpack(Packet) ->
    call(unpack, Version, [Packet]).

pack(Header, Packet) ->
    call(pack, Version, [Header, Packet]).

call(_Fun, 0, _Args) ->
    undefined;
call(Fun, Version, Args) ->
    case apply(mod_for(Version), Fun, Args) of
        undefined ->
            call(Fun, Version - 1, Args);
        Event ->
            Event
    end.
