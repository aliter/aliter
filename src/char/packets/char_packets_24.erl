-module(char_packets_24).

-include("include/records.hrl").

-export([unpack/1, pack/2]).

-define(WALKSPEED, 150).
-define(CHAR_BLOCK_SIZE, 136).


unpack(<<16#65:16/little,
         AccountID:32/little,
         LoginIDa:32/little,
         LoginIDb:32/little,
         _ClientType:16,
         Gender:8>>) ->
  {connect, AccountID, LoginIDa, LoginIDb, Gender};

unpack(<<16#66:16/little,
         Num:8/little>>) ->
  {choose, Num};

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
  { create,
    string:strip(binary_to_list(Name), both, 0),
    Str,
    Agi,
    Vit,
    Int,
    Dex,
    Luk,
    Num,
    HairColor,
    HairStyle
  };

unpack(<<16#68:16/little,
         CharacterID:32/little,
         EMail:40/little-binary-unit:8>>) ->
  { delete,
    CharacterID,
    string:strip(binary_to_list(EMail), both, 0)
  };

unpack(<<16#187:16/little,
         AccountID:32/little>>) ->
  {keepalive, AccountID};

unpack(<<16#28d:16/little,
         AccountID:32/little,
         CharacterID:32/little,
         NewName:24/little-binary-unit:8>>) ->
  { check_name,
    AccountID,
    CharacterID,
    string:strip(binary_to_list(NewName), both, 0)
  };

unpack(<<16#28f:16/little, CharacterID:32/little>>) ->
  {rename, CharacterID};

unpack(Unknown) ->
  log:warning("Got unknown data.", [{data, Unknown}]),
  unknown.


pack(
    characters,
    {Characters, MaxSlots, AvailableSlots, PremiumSlots}) ->
  [ <<16#6b:16/little,
      (length(Characters) * ?CHAR_BLOCK_SIZE + 27):16/little,
      MaxSlots:8/little,
      AvailableSlots:8/little,
      PremiumSlots:8/little>>,
    binary:copy(<<0>>, 20)
  ] ++ [character(C) || C <- Characters];

pack(pin_code, AccountID) ->
  <<16#8B9:16/little,
    0:16/little,
    0:16/little,
    AccountID:32/little,
    0:16/little>>;

pack(refuse, Reason) ->
  <<16#6c:16/little, Reason:8/little>>;

pack(character_created, Character) ->
  [<<16#6d:16/little>>, character(Character)];

pack(creation_failed, Reason) ->
  <<16#6e:16/little, Reason:8/little>>;

pack(character_deleted, ok) ->
  <<16#6f:16/little>>;

pack(deletion_failed, Reason) ->
  <<16#70:16/little, Reason:8/little>>;

pack(
    zone_connect,
    { #char{id = ID, map = Map},
      {ZA, ZB, ZC, ZD},
      ZonePort
    }) ->
  [ <<16#71:16/little, ID:32/little>>,
    pad_to([Map, <<".gat">>], 16),
    <<ZA, ZB, ZC, ZD, ZonePort:16/little>>
  ];

pack(name_check_result, Result) ->
  <<16#28e:16/little, Result:16/little>>;

pack(rename_result, Result) ->
  <<16#290:16/little, Result:16/little>>;

pack(Header, Data) ->
  log:error(
    "Cannot pack unknown data.",
    [ {header, Header}, {data, Data}]
  ),

  <<>>.


% NOTE: update CHAR_BLOCK_SIZE after changing this
character(C) ->
  [ <<(C#char.id):32/little,
      (C#char.base_exp):32/little,
      (C#char.zeny):32/little,
      (C#char.job_exp):32/little,
      (C#char.job_level):32/little,
      0:32/little, % TODO (Body state)
      0:32/little, % TODO (Health state)
      (C#char.effects):32/little,
      (C#char.karma):32/little,
      (C#char.manner):32/little,
      (C#char.status_points):16/little,
      (C#char.hp):32/little,
      (C#char.max_hp):32/little,
      (C#char.sp):16/little,
      (C#char.max_sp):16/little,
      ?WALKSPEED:16/little, % TODO (Walk speed)
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

    pad_to(C#char.name, 24),

    <<(C#char.str):8,
      (C#char.agi):8,
      (C#char.vit):8,
      (C#char.int):8,
      (C#char.dex):8,
      (C#char.luk):8,
      (C#char.num):16/little,
      % TODO: I think this is 0 for renamed
      1:16/little>>,

    pad_to([C#char.map, <<".gat">>], 16),

    <<0:32/little,  % delete date
      0:32/little>>  % robe
      % 0:32/little,  % change slot (0 = disabled)
      % 0:32/little>> % unknown (0 = disabled)
  ].

pad_to(Bin, Size) ->
  Binary = iolist_to_binary(Bin),
  [ binary:part(Binary, 0, min(byte_size(Binary), Size)),
    binary:copy(<<0>>, Size - byte_size(Binary))
  ].
