ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)

all: compile

compile:
	@./rebar compile
