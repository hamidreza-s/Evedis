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

test: compile
	@mkdir -p ./test/logs
	@exec ct_run -dir ./test -include ./include \
		-pa ./ebin -logdir ./test/logs \
		-suite evedis_SUITE \
		-suite evedis_hash_SUITE \
		-suite evedis_kv_SUITE \
		-suite evedis_list_SUITE \
		-suite evedis_set_SUITE \
		-suite evedis_string_SUITE

