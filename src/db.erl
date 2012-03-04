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


save_account(C, Account) ->
  ID =
    case Account#account.id of
      undefined -> redis:incr(C, "accounts:id");
      X -> X
    end,

  %redis:multi(C),

  log:debug("Creating account.",
    [{id, ID}, {account_id, Account#account.id}]),

  Hash = "account:" ++ integer_to_list(ID),
  redis:hset(C, Hash, "login", Account#account.login),
  redis:hset(C, Hash, "password", Account#account.password),
  redis:hset(C, Hash, "email", Account#account.email),
  redis:hset(C, Hash, "gender", Account#account.gender),
  redis:hset(C, Hash, "login_count", Account#account.login_count),
  redis:hset(C, Hash, "last_login", Account#account.last_login),
  redis:hset(C, Hash, "last_ip", Account#account.last_ip),
  redis:hset(C, Hash, "gm_level", Account#account.gm_level),

  redis:set(C, ["account:", Account#account.login], ID),

  %redis:exec(C),

  Account#account{id = ID}.


get_account(C, ID) ->
  Hash = ["account:", integer_to_list(ID)],
  #account{
    id = ID,
    login = gethash(C, Hash, "login"),
    password = gethash(C, Hash, "password"),
    email = gethash(C, Hash, "email"),
    gender = numeric(gethash(C, Hash, "gender")),
    login_count = numeric(gethash(C, Hash, "login_count")),
    last_login = numeric(gethash(C, Hash, "last_login")),
    last_ip = gethash(C, Hash, "last_ip"),
    gm_level = numeric(gethash(C, Hash, "gm_level"))
  }.


get_account_id(C, Name) ->
  case redis:get(C, ["account:", Name]) of
    undefined -> nil;
    {ok, X} -> numeric(X)
  end.


save_char(C, Char) ->
  ID =
    case Char#char.id of
      undefined -> redis:incr(C, "chars:id");
      X -> X
    end,

  %redis:multi(C),

  Hash = "char:" ++ integer_to_list(ID),
  redis:hset(C, Hash, "num", Char#char.num),
  redis:hset(C, Hash, "name", Char#char.name),
  redis:hset(C, Hash, "job", Char#char.job),
  redis:hset(C, Hash, "base_level", Char#char.base_level),
  redis:hset(C, Hash, "base_exp", Char#char.base_exp),
  redis:hset(C, Hash, "job_level", Char#char.job_level),
  redis:hset(C, Hash, "job_exp", Char#char.job_exp),
  redis:hset(C, Hash, "zeny", Char#char.zeny),
  redis:hset(C, Hash, "str", Char#char.str),
  redis:hset(C, Hash, "agi", Char#char.agi),
  redis:hset(C, Hash, "vit", Char#char.vit),
  redis:hset(C, Hash, "int", Char#char.int),
  redis:hset(C, Hash, "dex", Char#char.dex),
  redis:hset(C, Hash, "luk", Char#char.luk),
  redis:hset(C, Hash, "max_hp", Char#char.max_hp),
  redis:hset(C, Hash, "hp", Char#char.hp),
  redis:hset(C, Hash, "max_sp", Char#char.max_sp),
  redis:hset(C, Hash, "sp", Char#char.sp),
  redis:hset(C, Hash, "status_points", Char#char.status_points),
  redis:hset(C, Hash, "skill_points", Char#char.skill_points),
  redis:hset(C, Hash, "hair_style", Char#char.hair_style),
  redis:hset(C, Hash, "hair_colour", Char#char.hair_colour),
  redis:hset(C, Hash, "clothes_colour", Char#char.clothes_colour),
  redis:hset(C, Hash, "view_weapon", Char#char.view_weapon),
  redis:hset(C, Hash, "view_shield", Char#char.view_shield),
  redis:hset(C, Hash, "view_head_top", Char#char.view_head_top),
  redis:hset(C, Hash, "view_head_middle", Char#char.view_head_middle),
  redis:hset(C, Hash, "view_head_bottom", Char#char.view_head_bottom),
  redis:hset(C, Hash, "map", Char#char.map),
  redis:hset(C, Hash, "x", Char#char.x),
  redis:hset(C, Hash, "y", Char#char.y),
  redis:hset(C, Hash, "save_map", Char#char.save_map),
  redis:hset(C, Hash, "save_x", Char#char.save_x),
  redis:hset(C, Hash, "save_y", Char#char.save_y),
  redis:hset(C, Hash, "online", Char#char.online),
  redis:hset(C, Hash, "effects", Char#char.effects),
  redis:hset(C, Hash, "karma", Char#char.karma),
  redis:hset(C, Hash, "manner", Char#char.manner),
  redis:hset(C, Hash, "fame", Char#char.fame),
  redis:hset(C, Hash, "guild_position", Char#char.guild_position),
  redis:hset(C, Hash, "guild_taxed", Char#char.guild_taxed),
  redis:hset(C, Hash, "renamed", Char#char.renamed),
  redis:hset(C, Hash, "account_id", Char#char.account_id),
  redis:hset(C, Hash, "party_id", Char#char.party_id),
  redis:hset(C, Hash, "guild_id", Char#char.guild_id),
  redis:hset(C, Hash, "pet_id", Char#char.pet_id),
  redis:hset(C, Hash, "homunculus_id", Char#char.homunculus_id),
  redis:hset(C, Hash, "mercenary_id", Char#char.mercenary_id),

  redis:set(C, ["char:", Char#char.name], ID),

  redis:hset(
    C,
    ["account:", integer_to_list(Char#char.account_id), ":chars"],
    Char#char.num,
    ID
  ),

  %redis:exec(C),

  Char#char{id = ID}.


delete_char(C, Char) ->
  %redis:multi(C),

  Hash = "char:" ++ integer_to_list(Char#char.id),
  redis:del(C, Hash),
  redis:del(C, ["char:", Char#char.name]),
  redis:hdel(
    C,
    ["account:", integer_to_list(Char#char.account_id), ":chars"],
    Char#char.num
  ),

  %redis:exec(C),

  ok.


get_char(C, ID) ->
  Hash = "char:" ++ integer_to_list(ID),
  #char{
    id = ID,
    num = numeric(gethash(C, Hash, "num")),
    name = gethash(C, Hash, "name"),
    job = numeric(gethash(C, Hash, "job")),
    base_level = numeric(gethash(C, Hash, "base_level")),
    base_exp = numeric(gethash(C, Hash, "base_exp")),
    job_level = numeric(gethash(C, Hash, "job_level")),
    job_exp = numeric(gethash(C, Hash, "job_exp")),
    zeny = numeric(gethash(C, Hash, "zeny")),
    str = numeric(gethash(C, Hash, "str")),
    agi = numeric(gethash(C, Hash, "agi")),
    vit = numeric(gethash(C, Hash, "vit")),
    int = numeric(gethash(C, Hash, "int")),
    dex = numeric(gethash(C, Hash, "dex")),
    luk = numeric(gethash(C, Hash, "luk")),
    max_hp = numeric(gethash(C, Hash, "max_hp")),
    hp = numeric(gethash(C, Hash, "hp")),
    max_sp = numeric(gethash(C, Hash, "max_sp")),
    sp = numeric(gethash(C, Hash, "sp")),
    status_points = numeric(gethash(C, Hash, "status_points")),
    skill_points = numeric(gethash(C, Hash, "skill_points")),
    hair_style = numeric(gethash(C, Hash, "hair_style")),
    hair_colour = numeric(gethash(C, Hash, "hair_colour")),
    clothes_colour = numeric(gethash(C, Hash, "clothes_colour")),
    view_weapon = numeric(gethash(C, Hash, "view_weapon")),
    view_shield = numeric(gethash(C, Hash, "view_shield")),
    view_head_top = numeric(gethash(C, Hash, "view_head_top")),
    view_head_middle = numeric(gethash(C, Hash, "view_head_middle")),
    view_head_bottom = numeric(gethash(C, Hash, "view_head_bottom")),
    map = gethash(C, Hash, "map"),
    x = numeric(gethash(C, Hash, "x")),
    y = numeric(gethash(C, Hash, "y")),
    save_map = gethash(C, Hash, "save_map"),
    save_x = numeric(gethash(C, Hash, "save_x")),
    save_y = numeric(gethash(C, Hash, "save_y")),
    online = numeric(gethash(C, Hash, "online")),
    effects = numeric(gethash(C, Hash, "effects")),
    karma = numeric(gethash(C, Hash, "karma")),
    manner = numeric(gethash(C, Hash, "manner")),
    fame = numeric(gethash(C, Hash, "fame")),
    guild_position = numeric(gethash(C, Hash, "guild_position")),
    guild_taxed = numeric(gethash(C, Hash, "guild_taxed")),
    renamed = numeric(gethash(C, Hash, "renamed")),
    account_id = numeric(gethash(C, Hash, "account_id")),
    party_id = numeric(gethash(C, Hash, "party_id")),
    guild_id = numeric(gethash(C, Hash, "guild_id")),
    pet_id = numeric(gethash(C, Hash, "pet_id")),
    homunculus_id = numeric(gethash(C, Hash, "homunculus_id")),
    mercenary_id = numeric(gethash(C, Hash, "mercenary_id"))
  }.


get_account_chars(C, AccountID) ->
  Chars =
    redis:hgetall(
      C,
      ["account:", integer_to_list(AccountID), ":chars"]
    ),

  [db:get_char(C, numeric(ID)) || {_, ID} <- Chars].


get_account_char(C, AccountID, Num) ->
  ID =
    redis:hget(
      C,
      ["account:", integer_to_list(AccountID), ":chars"],
      integer_to_list(Num)
    ),

  case ID of
    undefined -> nil;
    {ok, X} -> db:get_char(C, numeric(X))
  end.


get_char_id(C, Name) ->
  case redis:get(C, ["char:", Name]) of
    undefined -> nil;
    {ok, X} -> numeric(X)
  end.


rename_char(C, ID, OldName, NewName) ->
  %redis:multi(C),

  Hash = "char:" ++ integer_to_list(ID),
  redis:del(C, ["char:", OldName]),
  redis:set(C, ["char:", NewName], ID),
  redis:hset(C, Hash, "name", NewName),
  redis:hset(C, Hash, "renamed", 1),
  
  %redis:exec(C),

  ok.


save_guild(C, Guild) ->
  ID =
    case Guild#guild.id of
      undefined -> redis:incr(C, "guilds:id");
      X -> X
    end,

  %redis:multi(C),

  Hash = "guild:" ++ integer_to_list(ID),
  redis:hset(C, Hash, "name", Guild#guild.name),
  redis:hset(C, Hash, "level", Guild#guild.level),
  redis:hset(C, Hash, "capacity", Guild#guild.capacity),
  redis:hset(C, Hash, "exp", Guild#guild.exp),
  redis:hset(C, Hash, "next_exp", Guild#guild.next_exp),
  redis:hset(C, Hash, "skill_points", Guild#guild.skill_points),
  redis:hset(C, Hash, "message_title", Guild#guild.message_title),
  redis:hset(C, Hash, "message_body", Guild#guild.message_body),
  redis:hset(C, Hash, "emblem", Guild#guild.emblem),
  redis:hset(C, Hash, "master_id", Guild#guild.master_id),

  redis:set(C, ["guild:", Guild#guild.name], ID),

  %redis:exec(C),

  Guild#guild{id = ID}.


delete_guild(C, Guild) ->
  %redis:multi(C),

  Hash = "guild:" ++ integer_to_list(Guild#guild.id),
  redis:del(C, Hash),
  redis:del(C, ["guild:", Guild#char.name]),

  %redis:exec(C),

  ok.


get_guild(C, ID) ->
  Hash = "guild:" ++ integer_to_list(ID),
  #guild{
    id = ID,
    name = gethash(C, Hash, "name"),
    level = numeric(gethash(C, Hash, "level")),
    capacity = numeric(gethash(C, Hash, "capacity")),
    exp = numeric(gethash(C, Hash, "exp")),
    next_exp = numeric(gethash(C, Hash, "next_exp")),
    skill_points = numeric(gethash(C, Hash, "skill_points")),
    message_title = gethash(C, Hash, "message_title"),
    message_body = gethash(C, Hash, "message_body"),
    emblem = gethash(C, Hash, "emblem"),
    master_id = numeric(gethash(C, Hash, "master_id"))
  }.


get_guild_id(C, Name) ->
  case redis:get(C, ["guild:", Name]) of
    undefined -> nil;
    {ok, X} -> numeric(X)
  end.


get_guild_master(C, Guild) ->
  ID =
    case Guild#guild.id of
      undefined -> redis:incr(C, "guilds:id");
      X -> X
    end,

  Hash = "guild:" ++ integer_to_list(ID),
  case redis:hget(C, Hash, "master_id") of
    undefined -> nil;
    {ok, Master} -> numeric(Master)
  end.


get_guild_members(C, GuildID) ->
  Chars =
    redis:lrange(
      C,
      ["guild:", integer_to_list(GuildID), ":members"],
      0,
      -1
    ),

  [db:get_char(C, numeric(ID)) || {_, ID} <- Chars].


add_char_to_guild(C, GuildID, CharacterID) ->
  redis:rpush(C, ["guild:", GuildID, ":members"], CharacterID).


delete_char_from_guild(C, GuildID, CharacterID) ->
  redis:lrem(C, ["guild:", GuildID, ":members"], 0, CharacterID),
  ok.


get_guild_relationships(C, GuildID) ->
  redis:hgetall(
    C,
    [ "guild:",
      integer_to_list(GuildID),
      ":relationships"
    ]
  ).


save_guild_relationship(C, GuildID, TargetID, Type) ->
  redis:hset(
    C,
    [ "guild:",
      integer_to_list(GuildID),
      ":relationships"
    ],
    integer_to_list(TargetID),
    integer_to_list(Type)
  ).


delete_guild_relationship(C, GuildID, TargetID) ->
  redis:hdel(
    C,
    [ "guild:",
      integer_to_list(GuildID),
      ":relationships"
    ],
    integer_to_list(TargetID)
  ),
  ok.


get_guild_relationship(C, GuildID, TargetID) ->
  numeric(
    gethash(
      C,
      [ "guild:",
        integer_to_list(GuildID),
        ":relationships"
      ],
      integer_to_list(TargetID)
    )
  ).


gethash(C, Key, Field) ->
  case redis:hget(C, Key, Field) of
    undefined -> error(["undefined", Key, Field]);
    {ok, V} -> V
  end.

% TODO: this is probably only called with one form
numeric(I) when is_binary(I) ->
	II = binary_to_list(I),
	try
		list_to_integer(II)
	catch
		error:badarg ->
			try list_to_float(II)
			catch error:badarg -> I
			end
	end;
numeric(I) when is_list(I) ->
	try
		list_to_integer(I)
	catch
		error:badarg ->
			try list_to_float(I)
			catch error:badarg -> I
			end
	end.
