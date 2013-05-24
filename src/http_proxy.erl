-module(http_proxy).
-export([start/0, start/2, init/3, handle/2, terminate/3]).

start() ->
    start(<<"localhost">>, 8081).

start(Host, Port) ->
  io:format("HTTP Proxy at ~p~p~n", [Host, Port]),
  Host2 = binary_to_list(Host),
  Dispatch = cowboy_router:compile([
    {Host2, [ {'_', ?MODULE, [] } ] }
  ]),
  cowboy:start_http(list_to_atom(Host2), 10,
    [{port, Port}],
    [{env, [{dispatch, Dispatch } ] }]
  ).


init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined}.

handle(Req, State) ->
    {Host, _ } = cowboy_req:host(Req),
    {ok, Req2 } = cowboy_req:reply(200, [], ["OK, HTTP for: ", binary_to_list(Host)], Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
