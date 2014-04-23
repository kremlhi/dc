dc.beam: dc.erl
	erlc +debug_info +warn_missing_spec $<

all: dc.beam

typecheck: all
	dialyzer -Wunmatched_returns -Werror_handling -Wrace_conditions \
	 -Wunderspecs --src dc.erl

test: typecheck
	erl -noshell -eval 'dc:test(), halt().'

clean:
	rm -f dc.beam
