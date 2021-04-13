REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)

get_deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

lint:
	elvis rock

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

test:
	$(REBAR) do eunit, proper, ct

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
