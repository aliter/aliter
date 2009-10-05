all: compile

compile:
	erl -pa ebin -make

start: compile
	erl -pa ebin -sname aliter -eval "application:start(aliter)."
