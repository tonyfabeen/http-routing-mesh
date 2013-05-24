all:
	test -d deps || rebar get-deps
	rebar compile
	@erl -noshell -pa './deps/cowboy/ebin' -pa './deps/ranch/ebin' -pa './ebin' -s http_routing_mesh start