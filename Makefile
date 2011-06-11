CC = gcc
CFLAGS = -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -lz
ERL = erl -pa ebin -pa lib/erldis/ebin -pa lib/erldis/deps/gen_server2/ebin -pa lib/elixir/ebin -pa lib/elixir/exbin
ERLANG = /usr/local/lib/erlang
OS = ${shell uname -s}

include arch/${OS}/Makefile

all: compile

compile:
	${CC} ${CFLAGS} ${ARCHFLAGS} -I${ERLANG}/usr/include/
	erl -pa ebin -make
	./lib/elixir/bin/elixirc -pa ebin lib/npc.ex -o ebin

clean:
	rm priv/nif.so
	rm ebin/*.beam

install: compile
	${ERL} -noshell -sname aliter -eval "aliter:install(), halt()."

uninstall:
	rm -R ~/.aliter

start: compile
	${ERL} -noshell -sname aliter -eval "application:start(sasl), application:start(aliter)."

configure: compile
	${ERL} -noshell -sname aliter -eval "config:setup(), halt()."
