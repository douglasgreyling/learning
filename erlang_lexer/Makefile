.PHONY: all compile test clean run

SRCDIR = src
TESTDIR = test
EBINDIR = ebin

SOURCES = $(wildcard $(SRCDIR)/*.erl)
TESTS = $(wildcard $(TESTDIR)/*.erl)
BEAMS = $(SOURCES:$(SRCDIR)/%.erl=$(EBINDIR)/%.beam)
TEST_BEAMS = $(TESTS:$(TESTDIR)/%.erl=$(EBINDIR)/%.beam)

ERLC_FLAGS = -I $(SRCDIR) -o $(EBINDIR)

all: compile

compile: $(EBINDIR) $(BEAMS)

$(EBINDIR):
	mkdir -p $(EBINDIR)

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl $(SRCDIR)/token.hrl
	erlc $(ERLC_FLAGS) $<

$(EBINDIR)/%.beam: $(TESTDIR)/%.erl $(SRCDIR)/token.hrl
	erlc $(ERLC_FLAGS) $<

test: compile $(TEST_BEAMS)
	erl -pa $(EBINDIR) -noshell -eval "eunit:test(lexer_test, [verbose])" -s init stop

run: compile
	erl -pa $(EBINDIR) -noshell -eval "main:start()" -s init stop

clean:
	rm -rf $(EBINDIR)/*.beam
