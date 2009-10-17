-module(login_packets).

-export([unpack/1, pack/2]).

% Login request
unpack(<<16#64:16/little,
        PacketVer:32/little,
        Login:24/little-binary-unit:8,
        Password:24/little-binary-unit:8,
        Region:8>>) ->
    {login,
     PacketVer,
     string:strip(erlang:binary_to_list(Login), right, 0),
     string:strip(erlang:binary_to_list(Password), right, 0),
     Region};

unpack(Unknown) ->
    log:warning("Got unknown data.", [{data, Unknown}]),
    false.

% Login accept
pack(16#69 = Header, {LoginIDa, LoginIDb, AccountID, Servers}) ->
    [<<Header:16/little,
       (length(Servers) * 32 + 47):16/little,
       LoginIDa:32/little,
       AccountID:32/little,
       LoginIDb:32/little,
       0:32>>,
     erlang:list_to_binary(lists:duplicate(24, 0)),
     <<0:16,
       1:8>>] ++
    lists:map(fun({{IA, IB, IC, ID}, Port, Name, 0, Maintenance, New}) ->
                  [<<IA, IB, IC, ID, Port:16/little>>,
                   erlang:list_to_binary(string:left(Name, 20, 0)),
                   <<0:16, Maintenance:16, New:16>>]
              end, Servers);

% Login denied
pack(16#6a = Header, {Type, S}) ->
    [<<Header:16/little,
       Type:8>>,
      (erlang:list_to_binary(string:left(S, 20, 0)))];

pack(Header, Data) ->
    log:error("Cannot pack unknown data.",
              [{header, Header},
               {data, Data}]),
    <<>>.
