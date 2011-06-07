-module(db).

-include("include/records.hrl").

-export([save_account/2, get_account/2]).


save_account(C, Account) ->
  ID =
    case Account#account.id of
      undefined -> erldis:incr(C, "accounts:id");
      X -> X
    end,

  Hash = "account:" ++ integer_to_list(ID),
  erldis:hset(C, Hash, "login", Account#account.login),
  erldis:hset(C, Hash, "password", Account#account.password),
  erldis:hset(C, Hash, "gender", Account#account.gender),
  erldis:hset(C, Hash, "login_count", Account#account.login_count),
  erldis:hset(C, Hash, "last_login", Account#account.last_login),
  erldis:hset(C, Hash, "gm_level", Account#account.gm_level),

  case Account#account.email of
    undefined -> ok;
    Email -> erldis:hset(C, Hash, "email", Email)
  end,

  case Account#account.last_ip of
    undefined -> ok;
    LastIP -> erldis:hset(C, Hash, "last_ip", LastIP)
  end,

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

