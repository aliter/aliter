-module(db).

-include("include/records.hrl").

-export([
    save_account/2,
    get_account/2,
    get_account_id/2]).

-export([
    save_char/2,
    delete_char/2,
    get_char/2,
    get_account_chars/2,
    get_account_char/3,
    get_char_id/2,
    rename_char/4]).

-export([
    save_guild/2,
    delete_guild/2,
    get_guild/2,
    get_guild_id/2,
    get_guild_master/2,
    get_guild_members/2,
    add_char_to_guild/3,
    delete_char_from_guild/3,
    get_guild_relationships/2]).

-export([
    save_guild_relationship/4,
    delete_guild_relationship/3,
    get_guild_relationship/3]).


save_account(C, X) -> erldis:exec(C, fun(D) -> save_account_(D, X) end).
save_account_(C, Account) ->
  ID =
    case Account#account.id of
      undefined -> erldis:incr(C, "accounts:id");
      X -> X
    end,

  Hash = "account:" ++ integer_to_list(ID),
  erldis:hset(C, Hash, "login", Account#account.login),
  erldis:hset(C, Hash, "password", Account#account.password),
  erldis:hset(C, Hash, "email", Account#account.email),
  erldis:hset(C, Hash, "gender", Account#account.gender),
  erldis:hset(C, Hash, "login_count", Account#account.login_count),
  erldis:hset(C, Hash, "last_login", Account#account.last_login),
  erldis:hset(C, Hash, "last_ip", Account#account.last_ip),
  erldis:hset(C, Hash, "gm_level", Account#account.gm_level),

  erldis:set(C, ["account:", Account#account.login], ID),

  Account#account{id = ID}.


get_account(C, ID) ->
  Hash = ["account:", integer_to_list(ID)],
  #account{
    id = ID,
    login = erldis:hget(C, Hash, "login"),
    password = erldis:hget(C, Hash, "password"),
    email = erldis:hget(C, Hash, "email"),
    gender = erldis:numeric(erldis:hget(C, Hash, "gender")),
    login_count = erldis:numeric(erldis:hget(C, Hash, "login_count")),
    last_login = erldis:numeric(erldis:hget(C, Hash, "last_login")),
    last_ip = erldis:hget(C, Hash, "last_ip"),
    gm_level = erldis:numeric(erldis:hget(C, Hash, "gm_level"))
  }.


get_account_id(C, Name) ->
  case erldis:get(C, ["account:", Name]) of
    nil -> nil;
    X -> erldis:numeric(X)
  end.


save_char(C, X) -> erldis:exec(C, fun(D) -> save_char_(D, X) end).
save_char_(C, Char) ->
  ID =
    case Char#char.id of
      undefined -> erldis:incr(C, "chars:id");
      X -> X
    end,

  Hash = "char:" ++ integer_to_list(ID),
  erldis:hset(C, Hash, "num", Char#char.num),
  erldis:hset(C, Hash, "name", Char#char.name),
  erldis:hset(C, Hash, "job", Char#char.job),
  erldis:hset(C, Hash, "base_level", Char#char.base_level),
  erldis:hset(C, Hash, "base_exp", Char#char.base_exp),
  erldis:hset(C, Hash, "job_level", Char#char.job_level),
  erldis:hset(C, Hash, "job_exp", Char#char.job_exp),
  erldis:hset(C, Hash, "zeny", Char#char.zeny),
  erldis:hset(C, Hash, "str", Char#char.str),
  erldis:hset(C, Hash, "agi", Char#char.agi),
  erldis:hset(C, Hash, "vit", Char#char.vit),
  erldis:hset(C, Hash, "int", Char#char.int),
  erldis:hset(C, Hash, "dex", Char#char.dex),
  erldis:hset(C, Hash, "luk", Char#char.luk),
  erldis:hset(C, Hash, "max_hp", Char#char.max_hp),
  erldis:hset(C, Hash, "hp", Char#char.hp),
  erldis:hset(C, Hash, "max_sp", Char#char.max_sp),
  erldis:hset(C, Hash, "sp", Char#char.sp),
  erldis:hset(C, Hash, "status_points", Char#char.status_points),
  erldis:hset(C, Hash, "skill_points", Char#char.skill_points),
  erldis:hset(C, Hash, "hair_style", Char#char.hair_style),
  erldis:hset(C, Hash, "hair_colour", Char#char.hair_colour),
  erldis:hset(C, Hash, "clothes_colour", Char#char.clothes_colour),
  erldis:hset(C, Hash, "view_weapon", Char#char.view_weapon),
  erldis:hset(C, Hash, "view_shield", Char#char.view_shield),
  erldis:hset(C, Hash, "view_head_top", Char#char.view_head_top),
  erldis:hset(C, Hash, "view_head_middle", Char#char.view_head_middle),
  erldis:hset(C, Hash, "view_head_bottom", Char#char.view_head_bottom),
  erldis:hset(C, Hash, "map", Char#char.map),
  erldis:hset(C, Hash, "x", Char#char.x),
  erldis:hset(C, Hash, "y", Char#char.y),
  erldis:hset(C, Hash, "save_map", Char#char.save_map),
  erldis:hset(C, Hash, "save_x", Char#char.save_x),
  erldis:hset(C, Hash, "save_y", Char#char.save_y),
  erldis:hset(C, Hash, "online", Char#char.online),
  erldis:hset(C, Hash, "effects", Char#char.effects),
  erldis:hset(C, Hash, "karma", Char#char.karma),
  erldis:hset(C, Hash, "manner", Char#char.manner),
  erldis:hset(C, Hash, "fame", Char#char.fame),
  erldis:hset(C, Hash, "guild_position", Char#char.guild_position),
  erldis:hset(C, Hash, "guild_taxed", Char#char.guild_taxed),
  erldis:hset(C, Hash, "renamed", Char#char.renamed),
  erldis:hset(C, Hash, "account_id", Char#char.account_id),
  erldis:hset(C, Hash, "party_id", Char#char.party_id),
  erldis:hset(C, Hash, "guild_id", Char#char.guild_id),
  erldis:hset(C, Hash, "pet_id", Char#char.pet_id),
  erldis:hset(C, Hash, "homunculus_id", Char#char.homunculus_id),
  erldis:hset(C, Hash, "mercenary_id", Char#char.mercenary_id),

  erldis:set(C, ["char:", Char#char.name], ID),

  erldis:hset(
    C,
    ["account:", integer_to_list(Char#char.account_id), ":chars"],
    Char#char.num,
    ID
  ),

  Char#char{id = ID}.


delete_char(C, X) -> erldis:exec(C, fun(D) -> delete_char_(D, X) end).
delete_char_(C, Char) ->
  Hash = "char:" ++ integer_to_list(Char#char.id),
  erldis:del(C, Hash),
  erldis:del(C, ["char:", Char#char.name]),
  erldis:hdel(
    C,
    ["account:", integer_to_list(Char#char.account_id), ":chars"],
    Char#char.num
  ),
  ok.


get_char(C, ID) ->
  Hash = "char:" ++ integer_to_list(ID),
  #char{
    id = ID,
    num = erldis:numeric(erldis:hget(C, Hash, "num")),
    name = erldis:hget(C, Hash, "name"),
    job = erldis:numeric(erldis:hget(C, Hash, "job")),
    base_level = erldis:numeric(erldis:hget(C, Hash, "base_level")),
    base_exp = erldis:numeric(erldis:hget(C, Hash, "base_exp")),
    job_level = erldis:numeric(erldis:hget(C, Hash, "job_level")),
    job_exp = erldis:numeric(erldis:hget(C, Hash, "job_exp")),
    zeny = erldis:numeric(erldis:hget(C, Hash, "zeny")),
    str = erldis:numeric(erldis:hget(C, Hash, "str")),
    agi = erldis:numeric(erldis:hget(C, Hash, "agi")),
    vit = erldis:numeric(erldis:hget(C, Hash, "vit")),
    int = erldis:numeric(erldis:hget(C, Hash, "int")),
    dex = erldis:numeric(erldis:hget(C, Hash, "dex")),
    luk = erldis:numeric(erldis:hget(C, Hash, "luk")),
    max_hp = erldis:numeric(erldis:hget(C, Hash, "max_hp")),
    hp = erldis:numeric(erldis:hget(C, Hash, "hp")),
    max_sp = erldis:numeric(erldis:hget(C, Hash, "max_sp")),
    sp = erldis:numeric(erldis:hget(C, Hash, "sp")),
    status_points = erldis:numeric(erldis:hget(C, Hash, "status_points")),
    skill_points = erldis:numeric(erldis:hget(C, Hash, "skill_points")),
    hair_style = erldis:numeric(erldis:hget(C, Hash, "hair_style")),
    hair_colour = erldis:numeric(erldis:hget(C, Hash, "hair_colour")),
    clothes_colour = erldis:numeric(erldis:hget(C, Hash, "clothes_colour")),
    view_weapon = erldis:numeric(erldis:hget(C, Hash, "view_weapon")),
    view_shield = erldis:numeric(erldis:hget(C, Hash, "view_shield")),
    view_head_top = erldis:numeric(erldis:hget(C, Hash, "view_head_top")),
    view_head_middle = erldis:numeric(erldis:hget(C, Hash, "view_head_middle")),
    view_head_bottom = erldis:numeric(erldis:hget(C, Hash, "view_head_bottom")),
    map = erldis:hget(C, Hash, "map"),
    x = erldis:numeric(erldis:hget(C, Hash, "x")),
    y = erldis:numeric(erldis:hget(C, Hash, "y")),
    save_map = erldis:hget(C, Hash, "save_map"),
    save_x = erldis:numeric(erldis:hget(C, Hash, "save_x")),
    save_y = erldis:numeric(erldis:hget(C, Hash, "save_y")),
    online = erldis:numeric(erldis:hget(C, Hash, "online")),
    effects = erldis:numeric(erldis:hget(C, Hash, "effects")),
    karma = erldis:numeric(erldis:hget(C, Hash, "karma")),
    manner = erldis:numeric(erldis:hget(C, Hash, "manner")),
    fame = erldis:numeric(erldis:hget(C, Hash, "fame")),
    guild_position = erldis:numeric(erldis:hget(C, Hash, "guild_position")),
    guild_taxed = erldis:numeric(erldis:hget(C, Hash, "guild_taxed")),
    renamed = erldis:numeric(erldis:hget(C, Hash, "renamed")),
    account_id = erldis:numeric(erldis:hget(C, Hash, "account_id")),
    party_id = erldis:numeric(erldis:hget(C, Hash, "party_id")),
    guild_id = erldis:numeric(erldis:hget(C, Hash, "guild_id")),
    pet_id = erldis:numeric(erldis:hget(C, Hash, "pet_id")),
    homunculus_id = erldis:numeric(erldis:hget(C, Hash, "homunculus_id")),
    mercenary_id = erldis:numeric(erldis:hget(C, Hash, "mercenary_id"))
  }.


get_account_chars(C, AccountID) ->
  Chars =
    erldis:hgetall(
      C,
      ["account:", integer_to_list(AccountID), ":chars"]
    ),

  [db:get_char(C, erldis:numeric(ID)) || {_, ID} <- Chars].


get_account_char(C, AccountID, Num) ->
  ID =
    erldis:hget(
      C,
      ["account:", integer_to_list(AccountID), ":chars"],
      integer_to_list(Num)
    ),

  case ID of
    nil -> nil;
    _ -> db:get_char(C, erldis:numeric(ID))
  end.


get_char_id(C, Name) ->
  case erldis:get(C, ["char:", Name]) of
    nil -> nil;
    X -> erldis:numeric(X)
  end.


rename_char(C, X, Y, Z) ->
  erldis:exec(C, fun(D) -> rename_char_(D, X, Y, Z) end).
rename_char_(C, ID, OldName, NewName) ->
  Hash = "char:" ++ integer_to_list(ID),
  erldis:del(C, ["char:", OldName]),
  erldis:set(C, ["char:", NewName], ID),
  erldis:hset(C, Hash, "name", NewName),
  erldis:hset(C, Hash, "renamed", 1).


save_guild(C, X) -> erldis:exec(C, fun(D) -> save_guild_(D, X) end).
save_guild_(C, Guild) ->
  ID =
    case Guild#guild.id of
      undefined -> erldis:incr(C, "guilds:id");
      X -> X
    end,

  Hash = "guild:" ++ integer_to_list(ID),
  erldis:hset(C, Hash, "name", Guild#guild.name),
  erldis:hset(C, Hash, "level", Guild#guild.level),
  erldis:hset(C, Hash, "capacity", Guild#guild.capacity),
  erldis:hset(C, Hash, "exp", Guild#guild.exp),
  erldis:hset(C, Hash, "next_exp", Guild#guild.next_exp),
  erldis:hset(C, Hash, "skill_points", Guild#guild.skill_points),
  erldis:hset(C, Hash, "message_title", Guild#guild.message_title),
  erldis:hset(C, Hash, "message_body", Guild#guild.message_body),
  erldis:hset(C, Hash, "emblem", Guild#guild.emblem),
  erldis:hset(C, Hash, "master_id", Guild#guild.master_id),

  erldis:set(C, ["guild:", Guild#guild.name], ID),

  Guild#guild{id = ID}.


delete_guild(C, X) -> erldis:exec(C, fun(D) -> delete_guild_(D, X) end).
delete_guild_(C, Guild) ->
  Hash = "guild:" ++ integer_to_list(Guild#guild.id),
  erldis:del(C, Hash),
  erldis:del(C, ["guild:", Guild#char.name]),
  ok.


get_guild(C, ID) ->
  Hash = "guild:" ++ integer_to_list(ID),
  #guild{
    id = ID,
    name = erldis:hget(C, Hash, "name"),
    level = erldis:numeric(erldis:hget(C, Hash, "level")),
    capacity = erldis:numeric(erldis:hget(C, Hash, "capacity")),
    exp = erldis:numeric(erldis:hget(C, Hash, "exp")),
    next_exp = erldis:numeric(erldis:hget(C, Hash, "next_exp")),
    skill_points = erldis:numeric(erldis:hget(C, Hash, "skill_points")),
    message_title = erldis:hget(C, Hash, "message_title"),
    message_body = erldis:hget(C, Hash, "message_body"),
    emblem = erldis:hget(C, Hash, "emblem"),
    master_id = erldis:numeric(erldis:hget(C, Hash, "master_id"))
  }.


get_guild_id(C, Name) ->
  case erldis:get(C, ["guild:", Name]) of
    nil -> nil;
    X -> erldis:numeric(X)
  end.


get_guild_master(C, Guild) ->
  ID =
    case Guild#guild.id of
      undefined -> erldis:incr(C, "guilds:id");
      X -> X
    end,

  Hash = "guild:" ++ integer_to_list(ID),
  case erldis:hget(C, Hash, "master_id") of
    nil -> nil;
    Master -> erldis:numeric(Master)
  end.


get_guild_members(C, GuildID) ->
  Chars =
    erldis:lrange(
      C,
      ["guild:", integer_to_list(GuildID), ":members"],
      0,
      -1
    ),

  [db:get_char(C, erldis:numeric(ID)) || {_, ID} <- Chars].


add_char_to_guild(C, GuildID, CharacterID) ->
  erldis:rpush(C, ["guild:", GuildID, ":members"], CharacterID).


delete_char_from_guild(C, GuildID, CharacterID) ->
  erldis:lrem(C, ["guild:", GuildID, ":members"], 0, CharacterID),
  ok.


get_guild_relationships(C, GuildID) ->
  erldis:hgetall(
    C,
    [ "guild:",
      integer_to_list(GuildID),
      ":relationships"
    ]
  ).


save_guild_relationship(C, GuildID, TargetID, Type) ->
  erldis:hset(
    C,
    [ "guild:",
      integer_to_list(GuildID),
      ":relationships"
    ],
    integer_to_list(TargetID),
    integer_to_list(Type)
  ).


delete_guild_relationship(C, GuildID, TargetID) ->
  erldis:hdel(
    C,
    [ "guild:",
      integer_to_list(GuildID),
      ":relationships"
    ],
    integer_to_list(TargetID)
  ),
  ok.


get_guild_relationship(C, GuildID, TargetID) ->
  erldis:numeric(
    erldis:hget(
      C,
      [ "guild:",
        integer_to_list(GuildID),
        ":relationships"
      ],
      integer_to_list(TargetID)
    )
  ).
