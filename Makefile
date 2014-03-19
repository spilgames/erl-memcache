REBAR ?= rebar
ifneq ($(wildcard rebar),)
	REBAR := ./rebar
endif

.PHONY: clean test doc

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
	$(REBAR) skip_deps=true xref

clean:
	$(REBAR) clean

test: compile
	$(REBAR) skip_deps=true eunit

docs: docsclean
	ln -s . doc/doc
	$(REBAR) skip_deps=true doc

docsclean:
	rm -f doc/*.html doc/*.css doc/erlang.png doc/edoc-info doc/doc

go:
	erl -name erl_memcache -pa deps/*/ebin -pa ebin/ -s memcache start ${EXTRA_ARGS}
