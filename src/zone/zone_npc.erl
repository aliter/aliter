-module(zone_npc).

-include("include/records.hrl").

-export([load_all/0]).

-export([say/3,
         close/2,
         menu/3,
         next/2]).

load_all() ->
    {ok, Scripts} = file:list_dir(config:home("scripts")),

    load_script(Scripts, 5000000).

load_script([], _N) ->
    [];
load_script([Script | Scripts], N) ->
    Dir = config:home("scripts/" ++ Script),

    log:debug("Loading script.",
              [{script, Dir ++ "/" ++ Script ++ ".lfe"}]),

    case lfe_io:read_file(Dir ++ "/" ++ Script ++ ".lfe") of
        {ok, [[defnpc | [Name | Attributes]] | Rest]} ->
            Includes = case lists:filter(fun(Sexp) -> lists:nth(1, Sexp) == include end, Rest) of
                           [[include, Files]] ->
                               lists:map(fun(File) ->
                                             ['include-file', (Dir ++ "/" ++ File)]
                                         end, Files);
                           _ ->
                               []
                       end,

            Attrs = [[define, ['npc-id'], N] |
                     lists:map(fun([K, V]) ->
                                   [define,
                                    [list_to_atom(lists:concat(["npc-", K]))],
                                    [quote, V]]
                               end, Attributes)],

            Body = lists:filter(fun(Sexp) -> lists:nth(1, Sexp) /= include end, Rest),

            Code = [[defmodule,
                     [Name, player],
                     [export, [main, 0]]] |
                     [([progn,
                        ['include-file', "lib/npc.lfh"]] ++ Includes ++ Attrs) |
                      Body]],

            case lfe_comp:forms(Script ++ ".lfe", Code, [return, {outdir, "ebin"}]) of
                {ok, Name, Warnings} ->
                    lists:map(fun({File, W}) ->
                                      log:warning("Errors occured while parsing script.",
                                                  [{script, File},
                                                   {warnings, W}])
                              end, Warnings),

                    Config = lists:map(fun list_to_tuple/1, Attributes),
                    [ #npc{id = N,
                           name = proplists:get_value(name, Config, "Unnamed"),
                           sprite = proplists:get_value(sprite, Config, 46),
                           map = proplists:get_value(map, Config, "prontera"),
                           coordinates = proplists:get_value(coodinates, Config, {155, 186}),
                           direction = proplists:get_value(direction, Config, southwest),
                           main = Name} | load_script(Scripts, N + 1) ];
                {error, Errors, Warnings} ->
                    lists:map(fun({File, E}) ->
                                  log:error("Errors occured while parsing script.",
                                            [{script, File},
                                             {errors, E}])
                              end, Errors),

                    lists:map(fun({File, W}) ->
                                      log:warning("Errors occured while parsing script.",
                                                  [{script, File},
                                                   {warnings, W}])
                              end, Warnings),

                    exit(normal)
            end;
        {error, ErrorInfo} ->
            lists:map(fun({_, E}) ->
                          log:error("Errors occured while parsing script.",
                                    [{script, Script},
                                     {errors, E}])
                      end, ErrorInfo),

            exit(normal)
    end.

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
