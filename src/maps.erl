-module(maps).

-include("include/records.hrl").

-export([read_cache/1,
         at/2,
         pretty/2,
         pathfind/3,
         simple_pathfind/3,
         complex_pathfind/3]).


read_maps(<<>>) ->
    [];
read_maps(<<Name:12/binary-unit:8,
            Width:16/little,
            Height:16/little,
            Length:32/little,
            Cells:Length/binary,
            Rest/binary>>) ->
    [#map{name = string:strip(binary_to_list(Name), right, 0),
          width = Width,
          height = Height,
          cells = zlib:uncompress(Cells)} | read_maps(Rest)].

read_cache(Filename) ->
    {ok, Cache} = file:read_file(Filename),

    <<_Size:32/little,
      _NumMaps:16/little,
      _:16,
      Maps/binary>> = Cache,

    read_maps(Maps).

flip(<<>>, _Width) ->
    <<>>;
flip(Tiles, Width) ->
    <<Cells:Width/binary, Rest/binary>> = Tiles,
    <<(flip(Rest, Width))/binary,
      Cells/binary>>.

at(M, {X, Y}) ->
    Skip = M#map.width * Y + X,

    case M#map.cells of
        <<_:Skip/binary, Cell, _/binary>> ->
            {ok, Cell};
        _OutOfBounds ->
            {error, unbounded}
    end.

pretty(Map) ->
    pretty(flip(Map#map.cells, Map#map.width)).

pretty(<<>>, _Width) ->
    ok;
pretty(Cells, Width) ->
    case Cells of
        <<Tiles:Width/binary, Rest/binary>> ->
            io:write(Tiles),
            io:format("~n"),
            pretty(Rest, Width);
        Invalid ->
            io:write(Invalid),
            io:format("~n"),
            log:error("Pretty failed.")
    end.


pathfind(Map, {X, Y}, {ToX, ToY}) ->
    Simple = simple_pathfind(Map, {X, Y}, {ToX, ToY}),
    Final = lists:last([{X, Y, straight} | Simple]),

    case Final of
        {ToX, ToY, _Dir} ->
            Simple;
        Stuck ->
            log:error("Final not final.", [{final, Final}, {wanted, {ToX, ToY, straight}}]),
            [] % Simple ++ complex_pathfind(Map, Stuck, {ToX, ToY})
    end.

simple_pathfind(_Map, {X, Y}, {ToX, ToY}) when X == ToX, Y == ToY ->
    [];
simple_pathfind(Map, {X, Y}, {ToX, ToY}) ->
    {NewX, NewY} = {X + sign(ToX, X), Y + sign(ToY, Y)},
    New = {NewX,
           NewY,
           if
               NewX /= X, NewY /= Y ->
                   diagonal;
               true ->
                   straight
           end},

    case at(Map, {NewX, NewY}) of
        {ok, 0} ->
            log:warning("Tile OK.",
                        [{coord, New}]),

            [New | simple_pathfind(Map, {NewX, NewY}, {ToX, ToY})];
        {ok, Tile} ->
            log:error("Cannot walk on tile; stopping.",
                      [{coord, New},
                       {tile, Tile}]),

            [];
        {error, unbounded} ->
            log:error("Requested tile out of bounds.",
                      [{requested, New}]),

            []
    end.

complex_pathfind(Map, {X, Y}, {ToX, ToY}) ->
    [].


sign(A, B) when A == B ->
    0;
sign(A, B) when A > B ->
    1;
sign(A, B) ->
    -1.
