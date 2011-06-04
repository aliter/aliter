-module(zone_npc).

-include("include/records.hrl").

-export([load_all/0]).

-export([do_all/2,
         say/3,
         close/2,
         menu/3,
         next/2]).

load_all() ->
    log:debug("Loading scripts.",
        [{directory, config:scripts()}]),

    {ok, Scripts} = file:list_dir(config:scripts()),

    elixir:file("lib/npc.ex"),

    load_script(Scripts, 5000000).

load_script([], _N) ->
    [];
load_script([Script | Scripts], N) ->
    Dir = config:scripts(Script),

    log:debug("Loading script.", [{script, Dir ++ "/main.ex"}]),

    elixir:file(Dir ++ "/main.ex"),
    {ok, Config} = file:consult(Dir ++ "/npc.erl"),
    Name = proplists:get_value(name, Config, "Unnamed"),
    NPC = proplists:get_value(npc, Config, Name),
    {Object, _} = elixir:eval(NPC),

    [ #npc{id = N,
            name = Name,
            sprite = proplists:get_value(sprite, Config, 46),
            map = proplists:get_value(map, Config, "prontera"),
            coordinates = proplists:get_value(coodinates, Config, {155, 186}),
            direction = proplists:get_value(direction, Config, southwest),
            main = Object} | load_script(Scripts, N + 1) ].

do_all(FSM, Packets) ->
    gen_fsm:send_all_state_event(FSM, {send_packets, Packets}).

say(FSM, NPC, Message) ->
    gen_fsm:send_all_state_event(FSM,
                                 {send_packet,
                                  dialog,
                                  {NPC,
                                   Message}}).

menu(FSM, NPC, Choices) ->
    gen_fsm:send_all_state_event(FSM,
                                 {send_packet,
                                  dialog_menu,
                                  {NPC,
                                   Choices}}).

next(FSM, NPC) ->
    gen_fsm:send_all_state_event(FSM,
                                 {send_packet,
                                  dialog_next,
                                  NPC}).

close(FSM, NPC) ->
    gen_fsm:send_all_state_event(FSM,
                                 {send_packet,
                                  dialog_close,
                                  NPC}).
