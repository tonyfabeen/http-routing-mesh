-module(http_proxy).
-export([start/0, start/2, init/3, handle/2, terminate/3]).
-record(state, {client}).

start() ->
    start(<<"localhost">>, 8081).

start(Host, Port) ->
  Host2 = binary_to_list(Host),
  Dispatch = cowboy_router:compile([
    {Host2, [ {'_', ?MODULE, [] } ] }
  ]),
  cowboy:start_http(list_to_atom(Host2), 10,
    [{port, Port}],
    [{env, [{dispatch, Dispatch } ] }]
  ),

  io:format("HTTP Proxy at ~p ~p ~n", [Host, Port]).


%%
init({tcp, http}, Req, _Opts) ->
  {ok, Client} = cowboy_client:init([]),
  State = #state{client=Client},
  {ok, Req, State}.

%%
handle(Req, State) ->

    %%{Host, _ } = cowboy_req:host(Req),
    {Method, RequestUrl, Headers} = request_dump(Req),

    %% Send Request to App   
    {ok, Client2} = cowboy_client:request(Method, RequestUrl, Headers, State#state.client), 
    {ok, ResponseStatus, ResponseHeaders, Client3} = cowboy_client:response(Client2),
    {ok, ResponseBody, _Client4} = cowboy_client:stream_body(Client3),

    %% Send Response
    io:format("ResponseStatus ~p ~n", [ResponseStatus]),
    io:format("ResponseHeaders ~p ~n", [ResponseHeaders]),
    io:format("ResponseBody ~p ~n", [ResponseBody]),
    {ok, Req2 }   = cowboy_req:reply(ResponseStatus, ResponseHeaders, ResponseBody, Req),
    {ok, Req2, State}.


%%
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
  RequestPort = <<":4567">>,


  io:format("METHOD : ~p ~n", [Method]),
  io:format("HOST : ~p ~n", [Host]),
  io:format("HOST URL : ~p ~n", [HostUrl]),
  io:format("PORT : ~p ~n", [Port]),
  io:format("REQUEST_PORT : ~p ~n", [RequestPort]),
  io:format("HEADERS : ~p ~n", [Headers]),
  io:format("PATH INFO : ~p ~n", [PathInfo]),
  io:format("PATH : ~p ~n", [Path]),
  io:format("QUERY_STRING : ~p ~n", [QueryString]),
  io:format("QUERY_STRING VALUES : ~p ~n", [QueryStringVals]),

  RequestUrl = <<Host/binary, RequestPort/binary, Path/binary, QueryString/binary>>,
  {Method, RequestUrl, Headers}.


%%
%%parse_response(Client) ->

