-module(maps).

-include("include/records.hrl").

-export([read_cache/1,
         at/2,
         pretty/1,
         pretty/2]).


read_maps(Cache) ->
    read_maps(Cache, 0).

read_maps(<<>>, _N) ->
    [];
read_maps(<<Name:12/binary-unit:8,
            Width:16/little,
            Height:16/little,
            Length:32/little,
            Cells:Length/binary,
            Rest/binary>>,
          N) ->
    [#map{id = N,
          name = string:strip(binary_to_list(Name), right, 0),
          width = Width,
          height = Height,
          cells = zlib:uncompress(Cells)} | read_maps(Rest, N + 1)].

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
    pretty(flip(Map#map.cells, Map#map.width), Map#map.width).

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


