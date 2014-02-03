
.PHONY: clean test doc

all: get-deps compile

get-deps:
	rebar get-deps

compile:
	rebar compile
	rebar skip_deps=true xref

clean:
	rebar clean

test: compile
	rebar skip_deps=true eunit

docs: docsclean
	ln -s . doc/doc
	rebar skip_deps=true doc

docsclean:
	rm -f doc/*.html doc/*.css doc/erlang.png doc/edoc-info doc/doc

go:
	erl -name erl_memcache -pa deps/*/ebin -pa ebin/ -s memcache start ${EXTRA_ARGS}
