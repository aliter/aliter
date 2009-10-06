-module(char_packets).

-include("include/records.hrl").

-export([unpack/1, pack/2]).

unpack(<<16#65:16/little,
         AccountID:32/little,
         LoginIDa:32/little,
         LoginIDb:32/little,
         _:16,
         Gender:8>>) ->
    {connect,
     AccountID,
     LoginIDa,
     LoginIDb,
     Gender};
unpack(<<16#66:16/little,
         Num:8/little>>) ->
    {choose,
     Num};
unpack(<<16#67:16/little,
         Name:24/little-binary-unit:8,
         Str:8,
         Agi:8,
         Vit:8,
         Int:8,
         Dex:8,
         Luk:8,
         Num:8,
         HairColor:16/little,
         HairStyle:16/little>>) ->
    {create, Name, Str, Agi, Vit, Int, Dex, Luk, Num, HairColor, HairStyle};
unpack(<<16#68:16/little,
         CharacterID:32/little,
         EMail:40/little-binary-unit:8>>) ->
    {delete, CharacterID, EMail};
unpack(<<16#187:16/little,
         AccountID:32/little>>) ->
    {keepalive, AccountID};
unpack(Unknown) ->
    log:warning("Got unknown data.", [{data, Unknown}]),
    false.

pack(16#6b, Characters) ->
    [<<16#6b:16/little,
       (length(Characters) * 112 + 24):16/little>>,
     list_to_binary(lists:duplicate(20, 0))] ++
    lists:map(fun(C) -> character(C) end, Characters);
pack(16#6c, Reason) ->
    <<16#6c:16/little,
      Reason:16/little>>;
pack(16#6d, Character) ->
    [<<16#6d>>,
     character(Character)];
pack(16#6e, Reason) ->
    <<16#6e:16/little,
      Reason:16/little>>;
pack(16#70, Reason) ->
    <<16#70:16/little,
      Reason:16/little>>;
pack(16#71, {#char{id = ID, map = Map}, {ZA, ZB, ZC, ZD}, ZonePort}) ->
    [<<16#71:16/little,
       ID:32/little>>,
     list_to_binary(string:left(Map ++ ".gat", 16, 0)),
     <<ZA, ZB, ZC, ZD, ZonePort:16/little>>];
pack(Header, Data) ->
    log:error("Cannot pack unknown data.",
               [{header, Header},
                {data, Data}]),
    false.


character(C) ->
    [<<(C#char.id):32/little,
       (C#char.base_exp):32/little,
       (C#char.zeny):32/little,
       (C#char.job_exp):32/little,
       (C#char.job_level):32/little,
       0:32/little, % ?
       0:32/little, % ?
       0:32/little, % (C#char.option):32/little,
       0:32/little, % (C#char.karma):32/little,
       0:32/little, % (C#char.manner):32/little,
       (C#char.status_points):16/little,
       (C#char.hp):32/little,
       (C#char.max_hp):32/little,
       (C#char.sp):16/little,
       (C#char.max_sp):16/little,
       140:16/little, % TODO (Walk speed)
       (C#char.job):16/little,
       (C#char.hair_style):16/little,
       (C#char.view_weapon):16/little,
       (C#char.base_level):16/little,
       (C#char.skill_points):16/little,
       (C#char.view_head_bottom):16/little,
       (C#char.view_shield):16/little,
       (C#char.view_head_top):16/little,
       (C#char.view_head_middle):16/little,
       (C#char.hair_colour):16/little,
       (C#char.clothes_colour):16/little>>,
     list_to_binary(string:left(C#char.name, 24, 0)),
     <<(C#char.str):8,
       (C#char.agi):8,
       (C#char.vit):8,
       (C#char.int):8,
       (C#char.dex):8,
       (C#char.luk):8,
       (C#char.num):16/little,
       (C#char.renamed):16/little>>]. % TODO (Renamed)
