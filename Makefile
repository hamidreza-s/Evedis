.PHONY: all compile deps clean live

all: compile

clean:
	@exec rebar clean

deps:
	@exec rebar get-deps

compile: deps
	@exec rebar compile

live: compile
	@exec erl -pa ./ebin -s evedis
