-module(zone_commands).

-include("include/records.hrl").

-export([parse/1,
         execute/3]).

parse(String) ->
    string:tokens(String, " ").

execute("caps", Args, #zone_state{tcp = TCP,
                                  map_server = MapServer,
                                  account = #account{id = AccountID},
                                  char = #char{id = CharacterID,
                                               x = X,
                                               y = Y}}) ->
    Capitalized = string:to_upper(string:join(Args, " ")),

    gen_server:cast(MapServer,
                    {send_to_other_players_in_sight,
                     {X, Y},
                     CharacterID,
                     16#8d,
                     {AccountID, Capitalized}}),

    TCP ! {16#8e, Capitalized};
execute(Unknown, Args, _State) ->
    log:warning("Unknown command.",
                [{command, Unknown},
                 {args, Args}]).
