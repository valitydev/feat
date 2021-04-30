REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
ELVIS := $(shell which elvis 2>/dev/null || which ./elvis)

.PHONY: get_deps ensure_elvis compile lint check_format \
    format test xref clean distclean dialyze plt_update

get_deps:
	$(REBAR) get-deps

ensure_elvis:
	bash -c "\
	if [ -z '$(ELVIS)' ]; then \
		git clone https://github.com/inaka/elvis.git elvis_repo && \
		cd elvis_repo && \
		rebar3 do compile, escriptize && \
		mv _build/default/bin/elvis ../ && \
		cd .. && \
		rm -rf elvis_repo; \
	else \
		:; \
	fi"

compile:
	$(REBAR) compile

lint:
	$(ELVIS) rock

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

test:
	$(REBAR) do eunit, proper

xref:
	$(REBAR) xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

dialyze:
	$(REBAR) dialyzer

plt_update:
	$(REBAR) dialyzer -u true -s false
