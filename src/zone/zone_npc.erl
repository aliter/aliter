-module(zone_npc).

-include("include/records.hrl").

-export([load_all/0]).

-export([
    do_all/2,
    say/3,
    close/2,
    menu/3,
    next/2]).


load_all() ->
  log:debug("Loading scripts.", [{directory, config:scripts()}]),
  Scripts = filelib:wildcard(config:scripts("*/init.ex")),
  load_scripts(Scripts).


load_scripts([]) ->
  ok;

load_scripts([Script | Scripts]) ->
  log:debug("Loading script.", [{script, Script}]),

  Dir = filename:dirname(Script),
  application:set_env(aliter, npc_path, Dir),
  elixir:file(Script),

  load_scripts(Scripts).


do_all(FSM, Packets) ->
  gen_fsm:send_all_state_event(FSM, {send_packets, Packets}).


say(FSM, NPC, Message) ->
  gen_fsm:send_all_state_event(FSM, {send_packet, dialog, {NPC, Message}}).


menu(FSM, NPC, Choices) ->
  gen_fsm:send_all_state_event(FSM, {send_packet, dialog_menu, {NPC, Choices}}).


next(FSM, NPC) ->
  gen_fsm:send_all_state_event(FSM, {send_packet, dialog_next, NPC}).


close(FSM, NPC) ->
  gen_fsm:send_all_state_event(FSM, {send_packet, dialog_close, NPC}).

