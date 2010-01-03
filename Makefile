ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)

all: erl

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam
