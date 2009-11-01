-module(login_packets).

-export([unpack/1, pack/2]).

% Login request
unpack(<<16#64:16/little,
        PacketVer:32/little,
        Login:24/little-binary-unit:8,
        Password:24/little-binary-unit:8,
        Region:8>>) ->
    Version = case PacketVer of
                  20090901 ->
                      24;
                  _ ->
                      PacketVer
              end,

    {login,
     Version,
     binary_to_string(Login),
     binary_to_string(Password),
     Region};

unpack(Unknown) ->
    log:warning("Got unknown data.", [{data, Unknown}]),
    false.

pack(accept, {LoginIDa, LoginIDb, AccountID, Servers}) ->
    [<<16#69:16/little,
       (length(Servers) * 32 + 47):16/little,
       LoginIDa:32/little,
       AccountID:32/little,
       LoginIDb:32/little,
       0:32>>,
     string_to_binary("", 24),
     <<0:16,
       1:8>>] ++
    lists:map(fun({{IA, IB, IC, ID}, Port, Name, 0, Maintenance, New}) ->
                  [<<IA, IB, IC, ID, Port:16/little>>,
                   string_to_binary(Name, 20),
                   <<0:16, Maintenance:16, New:16>>]
              end, Servers);
pack(refuse, {Reason, S}) ->
    [<<16#6a:16/little,
       Reason:8>>,
      string_to_binary(S, 20)];

pack(Header, Data) ->
    log:error("Cannot pack unknown data.",
              [{header, Header},
               {data, Data}]),
    <<>>.


% Util functions
binary_to_string(Binary) ->
    lists:takewhile(fun(C) ->
                        C /= 0
                    end,
                    binary_to_list(Binary)).

string_to_binary(String, Num) ->
    list_to_binary(string:left(String, Num, 0)).
