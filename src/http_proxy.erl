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
    {Host, _ }    = cowboy_req:host(Req),
    request_dump(Req),
    {ok, Req2 }   = cowboy_req:reply(200, [], ["OK, HTTP for: ", binary_to_list(Host)], Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


%% Extract request Values
request_dump(Req) ->
  {Method, _}  = cowboy_req:method(Req),
  {Host, _ }    = cowboy_req:host(Req),
  {HostUrl, _}  = cowboy_req:host_url(Req),
  {Port, _}     = cowboy_req:port(Req),
  {Headers, _}  = cowboy_req:headers(Req),
  {PathInfo, _} = cowboy_req:path_info(Req),
  {Path, _}     = cowboy_req:path(Req),
  {QueryString, _} = cowboy_req:qs(Req),
  {QueryStringVals, _} = cowboy_req:qs_vals(Req),

  io:format("METHOD : ~p ~n", [Method]),
  io:format("HOST : ~p ~n", [Host]),
  io:format("HOST URL : ~p ~n", [HostUrl]),
  io:format("PORT : ~p ~n", [Port]),
  io:format("HEADERS : ~p ~n", [Headers]),
  io:format("PATH INFO : ~p ~n", [PathInfo]),
  io:format("PATH : ~p ~n", [Path]),
  io:format("QUERY_STRING : ~p ~n", [QueryString]),
  io:format("QUERY_STRING VALUES : ~p ~n", [QueryStringVals]).




