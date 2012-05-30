.PHONY: deps test

all: deps compile

# Compiles the whole application
compile:
	rebar compile
	@rebar skip_deps=true xref | grep -v "is unused export (Xref)"

# Gets the dependencies to the deps folder. It does not try to compile them
getdeps:
	rebar get-deps

# Gets the dependencies to the deps folder. This is necessary for compile to succeed.
deps:
	rebar get-deps

# Cleans any generated files from the repo (except dependencies)
clean:
	rebar clean
	rm -f test/ebin/*
	rm -f doc/*.html doc/*.css doc/*.png doc/edoc-info

# Cleans any downloaded dependencies
distclean: clean
	rebar delete-deps

# Generates a release. Remember changing the release number in rel/reltool.config and src/service_profanityfilter.app.src
release: compile
	@mkdir -p /tmp/release_builder/
	@ln -s $(PWD)/. /tmp/release_builder/spapi_key_server
	rebar generate force=1
	@rm -fr /tmp/release_builder/*

# Runs every test suite under test/ abd generates an html page with detailed info about test coverage
test: compile
	rebar skip_deps=true eunit

# Generates the edoc documentation and places it under doc/ .
docs:
	rebar skip_deps=true doc

# Launches an erlang shell where the deps and the modules from the project are accesible
shell: compile
	rebar shell

# While developing with vi, :!make dialyzer | grep '%:t' can be used to run dialyzer in the current file
dialyzer: clean compile
	dialyzer -Wno_return -Wno_opaque -c ebin
	
typer: compile
	typer --show-exported -I include -I ../ src/*.erl
