-module(http_routing_mesh).
-export([start/0, init/3, handle/2, terminate/3]).

-record(state, {http_client, eredis}).

start() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowboy),
  application:start(http_routing_mesh).

%%
init({tcp, http}, Req, _Opts) ->
  {ok, Client} = cowboy_client:init([]),
  {ok, ERedis}  = eredis:start_link(),
  State = #state{http_client=Client, eredis=ERedis},
  {ok, Req, State}.

%%
handle(Req, State) ->
  io:format("HTTP Router PID : ~p ~n", [self()]),
  {Host, Req1} = cowboy_req:host(Req),
  case find_app_for(Host, State) of
    [AppUrl] ->
        delegate_request_to_app(Req1, AppUrl, State);
    [] ->
        io:format("App NOT FOUND ~n"),
        {ok, Req2 }   = cowboy_req:reply(404, [], "<h1>Sorry, App Not Found</h1>", Req1),
        {ok, Req2, State}
  end.

%%
terminate(_Reason, _Req, _State) ->
  ok.

%% Find Application
find_app_for(Host,State) ->
    {ok, AppUrl} = eredis:q(State#state.eredis, ["LRANGE", Host, "0", "0"]),
    AppUrl.


%% Delegate a Request to an application
delegate_request_to_app(Req, AppUrl, State) ->

    io:format("App is registered for => URL :  ~p ~n", [AppUrl]),

    %% Send Request to App
    {Method, RequestUrl, Headers} = request_dump(AppUrl, Req),
    {ok, Client2} = cowboy_client:request(Method, RequestUrl, Headers, State#state.http_client),
    {ok, ResponseStatus, ResponseHeaders, Client3} = cowboy_client:response(Client2),
    {ok, ResponseBody, _Client4} = cowboy_client:stream_body(Client3),

    %% Send Response
    io:format("ResponseStatus ~p ~n", [ResponseStatus]),
    io:format("ResponseHeaders ~p ~n", [ResponseHeaders]),
    io:format("ResponseBody ~p ~n", [ResponseBody]),

    {ok, Req2 }   = cowboy_req:reply(ResponseStatus, ResponseHeaders, ResponseBody, Req),
    {ok, Req2, State}.


%% Extract request Values
request_dump(AppUrl, Req) ->
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
  io:format("QUERY_STRING VALUES : ~p ~n", [QueryStringVals]),

  RequestUrl = <<AppUrl/binary, Path/binary, QueryString/binary>>,
  {Method, RequestUrl, Headers}.

