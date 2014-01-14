
.PHONY: clean test doc

all: get-deps compile

get-deps:
	rebar get-deps

compile:
	rebar compile xref

clean:
	rebar clean

test: compile
	rebar skip_deps=true eunit

docs: docsclean
	rebar skip_deps=true doc

docsclean:
	rm -f doc/*.html doc/*.css doc/*.png doc/edoc-info

go:
	erl -name erl_memcache -pa deps/*/ebin -pa ebin/ -s memcache start ${EXTRA_ARGS}
