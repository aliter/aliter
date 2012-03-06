-module(log).

-export([
    debug/1,
    debug/2,
    info/1,
    info/2,
    warning/1,
    warning/2,
    error/1,
    error/2,
    black/1,
    red/1,
    green/1,
    yellow/1,
    blue/1,
    magenta/1,
    cyan/1,
    white/1]).


format_of(A) when is_list(A) andalso hd(A) == 27 ->
  "~s";

format_of(_A) ->
  "~p".


log(Header, Msg, Args) ->
  Info = [{node, magenta(node())} | Args],

  Format = lists:map(fun
    ({Tag, _Val, F}) ->
      "~s  " ++ format_of(Tag) ++ ": " ++ F ++ "~n";
    ({Tag, Val}) ->
      "~s  " ++ format_of(Tag) ++ ": " ++ format_of(Val) ++ "~n";
    (Any) ->
      "~s  " ++ format_of(Any)
  end,
  Info),

  Vals = lists:map(fun
    ({Tag, Val, _F}) ->
      [lists:duplicate(length(Header) - 10, $ ), Tag, Val];
    ({Tag, Val}) ->
      [lists:duplicate(length(Header) - 10, $ ), Tag, Val];
    (Any) ->
      [lists:duplicate(length(Header) - 10, $ ), Any]
  end,
  Info),

  io:format(
    standard_error,
    lists:concat(["~s: ~s~n" | Format]),
    lists:concat([[Header, Msg] | Vals])).


debug(Msg) ->
  debug(Msg, []).


debug(Msg, Args) ->
  log(cyan("DEBUG"), Msg, Args).


info(Msg) ->
  info(Msg, []).


info(Msg, Args) ->
  log(green("INFO"), Msg, Args).


warning(Msg) ->
  warning(Msg, []).


warning(Msg, Args) ->
  log(yellow("WARNING"), Msg, Args).


error(Msg) ->
  log:error(Msg, []).


error(Msg, Args) ->
  log(red("ERROR"), Msg, Args).


colour(Num, String) ->
  lists:concat(["\e[3", Num, "m", String, "\e[39m"]).


black(String) ->
  colour(0, String).


red(String) ->
  colour(1, String).


green(String) ->
  colour(2, String).


yellow(String) ->
  colour(3, String).


blue(String) ->
  colour(4, String).


magenta(String) ->
  colour(5, String).


cyan(String) ->
  colour(6, String).


white(String) ->
  colour(7, String).

