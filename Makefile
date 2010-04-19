all: compile

compile:
	gcc -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -I/usr/lib/erlang/usr/include/
	erl -pa ebin -make

install: compile
	erl -noshell -pa ebin -sname aliter -eval "aliter:install()."

start: compile
	erl -noshell -pa ebin -sname aliter -eval "application:start(aliter)."
