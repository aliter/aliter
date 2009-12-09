all: compile

compile:
	gcc -arch i386 -fPIC -O2 -Wall -shared -flat_namespace -undefined suppress -o priv/nif.so src/nif.c -I/usr/local/lib/erlang/usr/include/
	erl -pa ebin -make

install: compile
	erl -noshell -pa ebin -sname aliter -eval "aliter:install()."

start: compile
	erl -noshell -pa ebin -sname aliter -eval "application:start(aliter)."
