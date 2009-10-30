all: compile

compile:
	gcc -o priv/extern -I/usr/lib/erlang/lib/erl_interface-3.6.1/include -L/usr/lib/erlang/lib/erl_interface-3.6.1/lib src/c/extern.c src/c/cnode.c -pthread -lerl_interface -lei -lz
	erl -pa ebin -make

install: compile
	erl -noshell -pa ebin -sname aliter -eval "aliter:install()."

start: compile
	erl -noshell -pa ebin -sname aliter -eval "application:start(aliter)."
