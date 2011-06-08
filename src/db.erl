-module(db).

-include("include/records.hrl").

-export([save_account/2, get_account/2]).

-export([
    save_char/2,
    get_char/2,
    get_account_chars/2,
    get_account_char/3]).

save_account(C, Account) ->
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
  Hash = ["account:", ID],
  #account{
    id = erldis:numeric(ID),
    login = erldis:hget(C, Hash, "login"),
    password = erldis:hget(C, Hash, "password"),
    email = erldis:hget(C, Hash, "email"),
    gender = erldis:numeric(erldis:hget(C, Hash, "gender")),
    login_count = erldis:numeric(erldis:hget(C, Hash, "login_count")),
    last_login = erldis:numeric(erldis:hget(C, Hash, "last_login")),
    last_ip = erldis:hget(C, Hash, "last_ip"),
    gm_level = erldis:numeric(erldis:hget(C, Hash, "gm_level"))
  }.


save_char(C, Char) ->
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


get_char(C, ID) ->
  Hash = "char:" ++ integer_to_list(ID),
  #char{
    id = erldis:numeric(ID),
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
