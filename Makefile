.PHONY: deps

all: deps compile

compile:
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps

eunit:
	./rebar skip_deps=true eunit
