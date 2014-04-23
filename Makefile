dc.beam: dc.erl
	erlc $<

all: dc.beam

test: all
	erl -noshell -eval 'dc:test(), halt().'

typecheck: all
	dialyzer dc.erl
