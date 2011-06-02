CC = gcc
ERLANG = /usr/local/lib/erlang
OS = $(shell uname -s)

all: compile

compile:
ifeq ($(OS), Darwin)
	${CC} -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -lz -flat_namespace -undefined suppress -I${ERLANG}/usr/include/
else
	${CC} -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -lz -I${ERLANG}/usr/include/
endif
	erl -pa ebin -make

clean:
	rm priv/nif.so
	rm ebin/*.beam

install: compile
	erl -noshell -pa ebin -sname aliter -eval "aliter:install(), halt()."

uninstall:
	rm -R ~/.aliter

start: compile
	erl -noshell -pa ebin -sname aliter -eval "application:start(aliter)."

configure: compile
	erl -noshell -pa ebin -sname aliter -eval "config:setup(), halt()."
