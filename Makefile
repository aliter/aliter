CC = gcc
ERLANG = /usr/local/lib/erlang

all: compile

compile:
	${CC} -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -lz -flat_namespace -undefined suppress -I${ERLANG}/usr/include/
	erl -pa ebin -make

clean:
    rm priv/nif.so
    rm ebin/*.beam

install: compile
	erl -noshell -pa ebin -sname aliter -eval "aliter:install()."

uninstall:
    rm -R ~/.aliter.db.*

start: compile
	erl -noshell -pa ebin -sname aliter -eval "application:start(aliter)."
