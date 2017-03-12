REBAR=rebar

compile:
	${REBAR} compile

clean:
	${REBAR} clean

bench:
	erl -pa ebin -pa deps/*/ebin -noshell -config benchmark/sys.config  -eval "load_test:bench(300000, 300)." -eval "init:stop()."

