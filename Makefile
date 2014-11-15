REBAR=./rebar

all: clean deps compile

deps: get-deps update-deps

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

run:
	erl -pa deps/*/ebin -pa deps/*/include -pa ebin -config test.config -s fluxer_app

console:
	erl -pa deps/*/ebin -pa deps/*/include -pa ebin

eunit:
	$(REBAR) eunit skip_deps=true

ct:
	$(REBAR) ct skip_deps=true
