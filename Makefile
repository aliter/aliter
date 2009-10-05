all: compile

compile:
	erl -pa ebin -make

start: compile
	erl -noshell -pa ebin -sname aliter -eval "application:start(aliter)."
