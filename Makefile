CC = gcc
CFLAGS = -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -lz
CONFIG_DIR = ~/.aliter
ERL = erl -pa ebin -pa scbin -pa lib/erlang-redis/ebin -pa lib/elixir/ebin -pa lib/elixir/exbin
ERLANG = /usr/local/lib/erlang
OS = ${shell uname -s}
SHA = ${shell git log -1 --pretty=format:%h}

include arch/${OS}/Makefile

all: compile

compile: update_submodules
	(cd lib/elixir && make)
	(cd lib/erlang-redis && make)
	${CC} ${CFLAGS} ${ARCHFLAGS} -I${ERLANG}/usr/include/
	erl -pa ebin -make
	./lib/elixir/bin/elixirc lib/npc.ex -o ebin

install: compile
	${ERL} -noshell -sname aliter -eval "aliter:install(), halt()."

uninstall:
	@@echo "Removing configuration directory:" ${CONFIG_DIR}
	@@rm -R ${CONFIG_DIR}

start: compile
	${ERL} -noshell -sname aliter -eval "application:start(sasl), application:start(aliter)."

shell: compile
	${ERL} -sname aliter

configure: compile
	${ERL} -noshell -sname aliter -eval "config:setup(), halt()."

clean:
	@@echo "Removing compiled modules..."
	@@(cd lib/elixir && make clean)
	@@(cd lib/erlang-redis && make clean)
	rm -f priv/nif.so
	rm -f ebin/*.beam

distclean: clean
	@@echo "Removing submodules..."
	@@rm -rf lib/elixir
	@@rm -rf lib/erlang-redis
	@@rm -rf lib/gen_nb_server

update_submodules:
	@@if [ -d .git ]; then \
		if git submodule status | grep -q -E '^-'; then \
			git submodule update --init --recursive; \
		else \
			git submodule update --init --recursive --merge; \
		fi; \
	fi;

# Update the submodules to the latest at the most logical branch.
pull_submodules:
	@@git submodule foreach "git pull \$$(git config remote.origin.url)"
	@@git submodule summary

pull: pull_submodules
	@@git pull ${REMOTE} ${BRANCH}

