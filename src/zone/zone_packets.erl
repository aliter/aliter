-module(zone_packets).

-include("include/records.hrl").

-export([unpack/1, pack/2]).

unpack(<<16#7d:16/little>>) ->
    map_loaded;
unpack(<<16#85:16/little,
         _:8,
         Head:8,
         _:16,
         Body:8>>) ->
    {change_direction, Head, Body};
unpack(<<16#89:16/little,
         _:40,
         Tick:32/little>>) ->
    {tick, Tick};
unpack(<<16#8c:16/little,
         _:40,
         ActorID:32/little>>) ->
    {request_name, ActorID};
unpack(<<16#a7:16/little,
         _:32,
         Position:3/little-binary-unit:8>>) ->
    {walk, decode_position(Position)};
unpack(<<16#bf:16/little,
         EmoticonID:8>>) ->
    {emotion, EmoticonID};
unpack(<<16#f3:16/little,
         Length:16/little,
         Message/little-binary-unit:8>>) when byte_size(Message) == (Length - 4) ->
    {speak, string:strip(binary_to_list(Message), right, 0)};
unpack(<<16#14d:16/little>>) ->
    request_guild_status;
unpack(<<16#14f:16/little,
         Page:32/little>>) ->
    {request_guild_info, Page};
unpack(<<16#18a:16/little,
         _:16>>) ->
    quit;
unpack(<<16#21d:16/little,
         Effect:32/little>>) ->
    {effect_state, Effect};
unpack(<<16#399:16/little,
         _Unknown:8>>) ->
    unknown;
unpack(<<16#436:16/little,
         AccountID:32/little,
         CharacterID:32/little,
         LoginIDa:32/little,
         _:32,
         Gender:8>>) ->
    {connect, AccountID, CharacterID, LoginIDa, Gender};
unpack(<<16#44a:16/little,
         _Unknown:32/little>>) ->
    unknown;
unpack(Unknown) ->
    log:warning("Got unknown data.", [{data, Unknown}]),
    unknown.

pack(16#73, {Tick, {X, Y, D}}) ->
    <<16#73:16/little,
      Tick:32/little,
      (encode_position(X, Y, D)):3/little-binary-unit:8>>;
pack(16#7f, Tick) ->
    <<16#7f:16/little, Tick:32/little>>;
pack(16#86, {ActorID, {{FromX, FromY}, {ToX, ToY}}, Tick}) ->
    <<16#86:16/little,
      ActorID:32/little,
      (encode_move(FromX, FromY, ToX, ToY)):6/little-binary-unit:8,
      Tick:32/little>>;
pack(16#87, {ActorID, {{FromX, FromY}, {ToX, ToY}}, Tick}) ->
    <<16#87:16/little,
      ActorID:32/little,
      (encode_move(FromX, FromY, ToX, ToY)):6/little-binary-unit:8,
      Tick:32/little>>;
pack(16#8d, {ActorID, Message}) ->
    <<16#8d:16/little,
      (length(Message) + 8):16/little,
      ActorID:32/little,
      Message/little-binary-unit:8>>;
pack(16#8e, Message) ->
    <<16#8e:16/little,
      (length(Message) + 4):16/little,
      Message/little-binary-unit:8>>;
pack(16#95, {ActorID, Name}) ->
    [<<16#95:16/little,
       ActorID:32/little>>,
     list_to_binary(string:left(Name, 24, 0))];
pack(16#c0, {ActorID, EmoticonID}) ->
    <<16#c0:16/little,
      ActorID:32/little,
      EmoticonID:8>>;
pack(16#18b, QuitResponse) ->
    <<16#18b:16/little,
      QuitResponse:8>>;
pack(16#195, {Name, Party, Guild, Position}) ->
    [<<16#195:16/little>>,
     list_to_binary(string:left(Name, 24, 0)),
     list_to_binary(string:left(Party, 24, 0)),
     list_to_binary(string:left(Guild, 24, 0)),
     list_to_binary(string:left(Position, 24, 0))];
pack(16#283, LoginIDa) ->
    <<16#283:16/little,
      LoginIDa:32/little>>;
pack(Header, Data) ->
    log:error("Cannot pack unknown data.",
               [{header, Header},
                {data, Data}]),
    <<>>.

decode_position(<<XNum, YNum, DNum>>) ->
    X = (XNum bsl 2) bor ((YNum band 16#C0) bsr 6),
    Y = ((YNum band 16#3F) bsl 4) bor ((DNum band 16#F0) bsr 4),
    D = DNum band 16#0F,
    {X, Y, D}.

encode_position(X, Y, D) ->
    A = (X bsr 2) band 16#FF,
    B = ((X bsl 6) bor ((X bsr 4) band 16#3f)) band 16#FF,
    C = ((Y bsl 4) bor (D band 16#0f)) band 16#FF,
    <<A, B, C>>.

encode_move(X, Y, ToX, ToY) ->
    A = (X bsr 2) band 16#FF,
    B = ((X bsl 6) bor ((X bsr 4) band 16#3f)) band 16#ff,
    C = ((Y bsl 4) bor ((ToX bsr 6) band 16#0f)) band 16#ff,
    D = ((ToX bsl 2) bor ((ToY bsr 8) band 16#03)) band 16#ff,
    E = ToY band 16#ff,
    F = ((8 bsl 4) bor (8 band 16#0f)) band 16#ff,
    <<A, B, C, D, E, F>>.

