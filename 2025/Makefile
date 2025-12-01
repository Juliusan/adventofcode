REBAR=rebar3
EUNIT_ARGS=

all: compile

compile:
	$(REBAR) compile

test:
	$(REBAR) eunit --verbose $(EUNIT_ARGS)

run:
	$(REBAR) as test shell --name "aoc_2025" --apps sync

.PHONY: all compile test run
