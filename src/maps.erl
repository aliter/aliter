-module(maps).

-include("include/records.hrl").

-export([read_cache/1, at/2, pretty/2]).


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
          cells = flip(zlib:uncompress(Cells), Width)} | read_maps(Rest)].

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
    <<(flip(Width, Rest))/binary,
      Cells/binary>>.

at(M, {X, Y}) ->
    Skip = M#map.width * Y + X,
    <<_:Skip/binary, Cell, _/binary>> = M#map.cells,
    Cell.

pretty(<<>>, _Width) ->
    ok;
pretty(Cells, Width) ->
    <<Tiles:Width/binary, Rest/binary>> = Cells,
    io:write(Tiles),
    io:format("~n"),
    pretty(Rest, Width).
