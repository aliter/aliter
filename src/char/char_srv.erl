-module(char_srv).
-behaviour(gen_server_tcp).

-include("include/records.hrl").

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    char_config:load(main),

    {ok, Name} = application:get_env(char, name),
    {ok, Port} = application:get_env(char, port),

    log:info("Starting character server.", [{name, Name}, {port, Port}]),

    application:set_env(mnesia,
                        dir,
                        lists:concat([aliter:home(), "database/char/", node()])),

    ok = mnesia:start(),

    ok = mnesia:wait_for_tables([char, ids], 2000),

    gen_server_tcp:start_link({local, server},
                              ?MODULE,
                              Port,
                              []).

init(Port) ->
    {ok, {Port, char_fsm, char_packets}, []}.

handle_call(Request, _From, State) ->
    log:debug("Character server got call.", [{call, Request}]),
    {reply, {illegal_request, Request}, State}.

handle_cast({#apirequest{add_char = C}, From}, State) when C =/= undefined ->
    log:debug("Character server got API request",
              [{request, log:yellow(add_char)}, {char, C}]),

    Create = fun() ->
                 Char = C#char{id = mnesia:dirty_update_counter(ids, char, 0)},
                 mnesia:write(Char),
                 mnesia:dirty_update_counter(ids, char, 1),
                 Char
             end,
    case mnesia:transaction(Create) of
        {atomic, Char} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
                                                          char = Char});
        {aborted, _Reason} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
                                                          info = "Character could not be created."})
    end,

    {noreply, State};
handle_cast({#apirequest{get_char = C}, From}, State) when C =/= undefined ->
    log:debug("Character server got API request",
              [{request, log:yellow(get_char)}, {char, C}]),

    Get = fun() -> mnesia:match_object(api:dc(C)) end,
    case mnesia:transaction(Get) of
        {atomic, Chars} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
                                                          char = Chars});
        {aborted, _Reason} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
                                                          info = "No characters found."})
    end,

    {noreply, State};
handle_cast({#apirequest{delete_char = C}, From}, State) when C =/= undefined ->
    log:debug("Character server got API request",
              [{request, log:yellow(delete_char)}, {char, C}]),

    Get = fun() ->
              Chars = mnesia:match_object(api:dc(C)),
              lists:foreach(fun mnesia:delete_object/1, Chars),
              Chars
          end,
    case mnesia:transaction(Get) of
        {atomic, Chars} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
                                                          info = "Characters deleted.",
                                                          char = Chars});
        {aborted, _Reason} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
                                                          info = "Character not found."})
    end,

    {noreply, State};
handle_cast({#apirequest{update_char = C}, From}, State) when C =/= undefined ->
    log:debug("Character server got API request",
              [{request, log:yellow(update_char)}, {char, C}]),

    Get = fun() -> mnesia:write(C) end,
    case mnesia:transaction(Get) of
        {atomic, ok} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "ok",
                                                          char = C});
        {aborted, _Reason} ->
            From ! api_pb:encode_apiresponse(#apiresponse{msg = "error",
                                                          info = "Account could not be updated."})
    end,

    {noreply, State};
handle_cast(Cast, State) ->
    log:debug("Character server got cast.", [{cast, Cast}]),
    {noreply, State}.

handle_info(Info, State) ->
    log:debug("Character server got info.", [{info, Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    log:info("Character server terminating."),
    mnesia:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
