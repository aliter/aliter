-module(zone_packets_24).

-include("include/records.hrl").

-export([unpack/1, pack/2]).

-define(WALKSPEED, 150).


unpack(<<16#7d:16/little>>) ->
    map_loaded;
unpack(<<16#85:16/little,
         _:16,
         Head:16/little,
         _:24,
         Body:8>>) ->
    {change_direction, Head, Body};
unpack(<<16#89:16/little,
         _:40,
         Tick:32/little>>) ->
    {tick, Tick};
unpack(<<16#90:16/little,
         NPCID:32/little,
         _:8>>) ->
    {npc_activate, NPCID};
unpack(<<16#8c:16/little,
         _:64,
         ActorID:32/little>>) ->
    {request_name, ActorID};
unpack(<<16#99:16/little,
         Length:16/little,
         Message/binary>>) when byte_size(Message) == (Length - 4) ->
    {broadcast, string:strip(binary_to_list(Message), right, 0)};
unpack(<<16#a7:16/little,
         _:32,
         Position:3/little-binary-unit:8>>) ->
    {walk, decode_position(Position)};
unpack(<<16#b8:16/little,
         ActorID:32/little,
         Selection:8>>) ->
    {npc_menu_select, ActorID, Selection};
unpack(<<16#b9:16/little,
         ActorID:32/little>>) ->
    {npc_next, ActorID};
unpack(<<16#bf:16/little,
         EmoticonID:8>>) ->
    {emotion, EmoticonID};
unpack(<<16#f3:16/little,
         Length:16/little,
         Message/little-binary-unit:8>>) when byte_size(Message) == (Length - 4) ->
    {speak, string:strip(binary_to_list(Message), right, 0)};
unpack(<<16#146:16/little,
         ActorID:32/little>>) ->
    {npc_close, ActorID};
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
      (encode_position(X, Y, D)):3/binary,
      0,
      0,
      0>>;
pack(16#78, N) ->
    {X, Y} = N#npc.coordinates,
    D = case N#npc.direction of
            north -> 0;
            northwest -> 1;
            west -> 2;
            southwest -> 3;
            south -> 4;
            southeast -> 5;
            east -> 6;
            northeast -> 7
        end,

    <<16#78:16/little,
      0:8, % Nothing
      (N#npc.id):32/little,
      200:16/little, % Static (walk speed)
      0:6/unit:8, % Nothing
      (N#npc.sprite):16/little,
      0:30/unit:8, % Nothing
      (encode_position(X, Y, D)):3/binary,
      0:3/unit:8, % Nothing
      1:8,
      0:8>>; % Static
pack(16#7f, Tick) ->
    <<16#7f:16/little, Tick:32/little>>;
pack(16#80, {ActorID, Type}) ->
    <<16#80:16/little,
      ActorID:32/little,
      Type:8>>;
pack(16#86, {ActorID, {FromX, FromY}, {ToX, ToY}, Tick}) ->
    <<16#86:16/little,
      ActorID:32/little,
      (encode_move(FromX, FromY, ToX, ToY)):6/little-binary-unit:8,
      Tick:32/little>>;
pack(16#87, {{FromX, FromY}, {ToX, ToY}, Tick}) ->
    <<16#87:16/little,
      Tick:32/little,
      (encode_move(FromX, FromY, ToX, ToY)):6/little-binary-unit:8>>;
pack(16#8d, {ActorID, Message}) ->
    [<<16#8d:16/little,
       (length(Message) + 9):16/little,
       ActorID:32/little>>,
     list_to_binary(Message),
     <<0>>];
pack(16#8e, Message) ->
    [<<16#8e:16/little,
       (length(Message) + 5):16/little>>,
     list_to_binary(Message),
     <<0>>];
pack(16#91, {Map, X, Y}) ->
    [<<16#91:16/little>>,
     list_to_binary(string:left(Map ++ ".gat", 16, 0)),
     <<X:16/little,
       Y:16/little>>];
pack(16#92, {Map, X, Y, {ZA, ZB, ZC, ZD}, Port}) ->
    [<<16#91:16/little>>,
     list_to_binary(string:left(Map ++ ".gat", 16, 0)),
     <<X:16/little,
       Y:16/little,
       ZA:8,
       ZB:8,
       ZC:8,
       ZD:8,
       Port:16/little>>];
pack(16#95, {ActorID, Name}) ->
    [<<16#95:16/little,
       ActorID:32/little>>,
     list_to_binary(string:left(Name, 24, 0))];
pack(16#9a, Message) ->
    [<<16#9a:16/little,
       (length(Message) + 5):16/little>>,
     list_to_binary(Message),
     <<0>>];
pack(16#9c, {ActorID, HeadDir, BodyDir}) ->
    <<16#9c:16/little,
      ActorID:32/little,
      HeadDir:16/little,
      BodyDir:8>>;
pack(16#b0, {Type, Value}) ->
    <<16#b0:16/little,
      Type:16/little,
      Value:32/little>>;
pack(16#b1, {Type, Value}) ->
    <<16#b1:16/little,
      Type:16/little,
      Value:32/little>>;
pack(16#b4, {NPCID, Message}) ->
    [<<16#b4:16/little,
       (length(Message) + 9):16/little,
       NPCID:32/little>>,
     list_to_binary(Message),
     <<0>>];
pack(16#b5, NPCID) ->
    <<16#b5:16/little,
      NPCID:32/little>>;
pack(16#b6, NPCID) ->
    <<16#b6:16/little,
      NPCID:32/little>>;
pack(16#b7, {NPCID, Choices}) ->
    Menu = string:join(Choices, ":"),
    [<<16#b7:16/little,
       (length(Menu) + 8):16/little,
       NPCID:32/little>>,
     list_to_binary(Menu)];
pack(16#bd, C) ->
    <<16#bd:16/little,
      (C#char.status_points):16/little,
      (C#char.str):8/little,
      0:8/little,
      (C#char.agi):8/little,
      0:8/little,
      (C#char.vit):8/little,
      0:8/little,
      (C#char.int):8/little,
      0:8/little,
      (C#char.dex):8/little,
      0:8/little,
      (C#char.luk):8/little,
      0:8/little,
      7:16/little,
      0:16/little,
      6:16/little,
      5:16/little,
      0:16/little,
      5:16/little,
      0:16/little,
      5:16/little,
      6:16/little,
      6:16/little,
      1:16/little,
      2:16/little,
      0:8, % Nothing
      0:8, % Nothing
      0:8, % Nothing, v-
      0:8>>; % TODO
pack(16#c0, {ActorID, EmoticonID}) ->
    <<16#c0:16/little,
      ActorID:32/little,
      EmoticonID:8>>;
pack(16#10f, Skills) ->
    [<<16#10f:16/little,
       (37 * length(Skills) + 4):16/little>>,
     lists:map(fun({ID, Type, Level, SP, Range, Name, Up}) ->
                   [<<ID:16/little,
                      Type:16/little,
                      0:16, % Nothing
                      Level:16/little,
                      Range:16/little,
                      SP:16/little>>,
                    list_to_binary(string:left(Name, 24, 0)),
                    <<Up>>]
               end, Skills)];
pack(16#13a, Range) ->
    <<16#13a:16/little,
      Range:16/little>>;
pack(16#141, {Stat, Value, Bonus}) ->
    <<16#141:16/little,
      Stat:32/little,
      Value:32/little,
      Bonus:32/little>>;
pack(16#14c, Relationships) ->
    [<<16#14c:16/little,
       (32 * length(Relationships) + 4):16/little>>,
     lists:map(fun(R) ->
                   [<<(R#guild_relationship.type):32/little,
                      (R#guild_relationship.b_id):32/little>>,
                    list_to_binary(string:left("Guild name here!", 24, 0))] % TODO: Guild name
               end,
               Relationships)];
pack(16#14e, State) ->
    case State of
        master ->
            <<16#14e:16/little,
              16#d7:32/little>>;
        _Other ->
            <<16#14e:16/little,
              16#57:32/little>>
    end;
pack(16#17f, Message) ->
    [<<16#17f:16/little,
       (length(Message) + 5):16/little>>,
     list_to_binary(Message),
     <<5>>];
pack(16#18b, QuitResponse) ->
    <<16#18b:16/little,
      QuitResponse:16/little>>;
pack(16#195, {AccountID, Name, Party, Guild, Position}) ->
    [<<16#195:16/little,
       AccountID:32/little>>,
     list_to_binary(string:left(Name, 24, 0)),
     list_to_binary(string:left(Party, 24, 0)),
     list_to_binary(string:left(Guild, 24, 0)),
     list_to_binary(string:left(Position, 24, 0))];
pack(16#1b6, Guild) ->
    [<<16#1b6:16/little,
       (Guild#guild.id):32/little,
       (Guild#guild.level):32/little,
       0:32/little, % TODO: Online count
       (16 * Guild#guild.level):32/little, % TODO: Verify this
       9001:32/little, % TODO: Average level
       (Guild#guild.exp):32/little,
       (Guild#guild.next_exp):32/little,
       0:32/little, % TODO: Tax points
       0:32/little, % TODO: Tendency Left/Right
       0:32/little, % TODO: Tendency Down/Up
       0:32/little>>, % TODO: Emblem ID
     list_to_binary(string:left(Guild#guild.name, 24, 0)),
     list_to_binary(string:left("Master goes here!", 24, 0)), % TODO: Master name
     list_to_binary(string:left("Everywhere, bitches.", 20, 0))]; % TODO: Territory
pack(16#1d7, Character) ->
    <<16#1d7:16/little,
      (Character#char.account_id):32/little,
      2:8, % Type
      (Character#char.view_weapon):16/little,
      (Character#char.view_shield):16/little>>;
pack(16#22b, {A, C}) ->
    Gender = if
                 A#account.gender == 0 ->
                     1;
                 true ->
                     0
             end,

    <<16#22b:16/little,
      (A#account.id):32/little,
      ?WALKSPEED:16/little, % TODO: Walk speed
      0:16/little, % TODO: Effect 1
      0:16/little, % TODO: Effect 2
      0:16/little, % TODO: Effect 3
      0:16, % Nothing
      (C#char.job):16/little,
      (C#char.hair_style):16/little,
      (C#char.view_weapon):16/little,
      (C#char.view_shield):16/little,
      (C#char.view_head_bottom):16/little,
      (C#char.view_head_top):16/little,
      (C#char.view_head_middle):16/little,
      (C#char.hair_colour):16/little,
      (C#char.clothes_colour):16/little,
      0:16/little, % TODO: Head direction (test this)
      (C#char.guild_id):32/little,
      (C#char.guild_id):16/little, % Guild emblem ID
      (C#char.manner):16/little, % Manners
      0:16/little, % Effect
      0:16, % Nothing,
      (C#char.karma):8, % Karma
      Gender:8, % Gender
      (encode_position(C#char.x, C#char.y, 0)):3/binary-unit:8,
      5:8,
      5:8,
      (C#char.base_level):16/little>>;
pack(16#22c, {A, C, Tick}) ->
    Gender = if
                 A#account.gender == 0 ->
                     1;
                 true ->
                     0
             end,

    <<16#22c:16/little,
      0:8, % Nothing
      (A#account.id):32/little,
      ?WALKSPEED:16/little, % TODO: Walk speed
      0:16/little, % TODO: Effect 1
      0:16/little, % TODO: Effect 2
      0:16/little, % TODO: Effect 3
      0:16, % Nothing
      (C#char.job):16/little,
      (C#char.hair_style):16/little,
      (C#char.view_weapon):16/little,
      (C#char.view_shield):16/little,
      (C#char.view_head_bottom):16/little,
      Tick:32/little,
      (C#char.view_head_top):16/little,
      (C#char.view_head_middle):16/little,
      (C#char.hair_colour):16/little,
      (C#char.clothes_colour):16/little,
      0:16/little, % TODO: Head direction (test this)
      (C#char.guild_id):32/little,
      (C#char.guild_id):16/little, % Guild emblem ID
      (C#char.manner):16/little, % Manners
      0:16/little, % Effect
      0:16, % Nothing,
      (C#char.karma):8, % Karma
      Gender:8, % Gender
      (encode_position(C#char.x, C#char.y, 0)):3/binary-unit:8,
      5:8,
      5:8,
      0:8/unit:3, % Nothing
      (C#char.base_level):16/little>>;
pack(16#283, LoginIDa) ->
    <<16#283:16/little,
      LoginIDa:32/little>>;
pack(16#2b9, _Hotkeys) ->
    [<<16#2b9:16/little>>,
     list_to_binary(lists:duplicate(189, 0))]; % TODO
pack(16#2c9, InvitationState) ->
    <<16#2c9:16/little,
      InvitationState:8>>;
pack(16#2d0, Equipment) ->
    <<16#2d0:16/little,
      (26 * length(Equipment) + 4):16/little>>; % TODO
pack(16#2da, ViewEquipState) ->
    <<16#2da:16/little,
      ViewEquipState:8>>;
pack(Header, Data) ->
    log:error("Cannot pack unknown data.",
               [{header, Header},
                {data, Data}]),
    <<>>.

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

