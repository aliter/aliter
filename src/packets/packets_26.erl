-module(packets_26).

-export([packet_size/1]).


packet_size(16#0436) -> 19;
packet_size(16#035F) -> 5;
packet_size(16#0360) -> 6;
packet_size(16#0361) -> 5;
packet_size(16#0362) -> 6;
packet_size(16#0363) -> 6;
packet_size(16#0364) -> 8;
packet_size(16#0365) -> 8;
packet_size(16#0366) -> 10;
packet_size(16#0367) -> 90;
packet_size(16#0368) -> 6;
packet_size(16#0369) -> 6;
packet_size(16#0856) -> 0;
packet_size(16#0857) -> 0;
packet_size(16#0858) -> 0;
packet_size(16#0859) -> 0;
packet_size(_Other) -> undefined.
