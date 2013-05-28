-module(http_routing_mesh).
-export([start/0, init/3, handle/2, terminate/3]).

-record(state, {client, applications, proxies}).

start() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowboy),
  N_acceptors = 10,
  Dispatch = cowboy_router:compile([
      {'_', [{'_', http_routing_mesh, []}]}
    ]),
  cowboy:start_http(http_routing_mesh, N_acceptors,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),

  io:format("HTTP Router Started at port : 8080 ~n").


%%
init({tcp, http}, Req, _Opts) ->
  {ok, Client} = cowboy_client:init([]),
  State = #state{client=Client, applications=setup_applications(), proxies=setup_proxies()},
  {ok, Req, State}.

%%
handle(Req, State) ->
  {Host, Req1} = cowboy_req:host(Req),
  delegate_to_app(Host, Req1, State).

%%
terminate(_Reason, _Req, _State) ->
  ok.

%% Find Application
delegate_to_app(Host, Req, State) ->
    case ets:lookup(State#state.applications, Host) of
        [App] ->
            {AppHost, AppPort, AppStatus} = App,
            io:format("App is registered for => URL :  ~p PORT: ~p STATUS: ~p ~n", [AppHost, AppPort, AppStatus]),

            %% Send Request to App
            {Method, RequestUrl, Headers} = request_dump(AppHost, AppPort, Req),
            {ok, Client2} = cowboy_client:request(Method, RequestUrl, Headers, State#state.client),
            {ok, ResponseStatus, ResponseHeaders, Client3} = cowboy_client:response(Client2),
            {ok, ResponseBody, _Client4} = cowboy_client:stream_body(Client3),

            %% Send Response
            io:format("ResponseStatus ~p ~n", [ResponseStatus]),
            io:format("ResponseHeaders ~p ~n", [ResponseHeaders]),
            io:format("ResponseBody ~p ~n", [ResponseBody]),
            {ok, Req2 }   = cowboy_req:reply(ResponseStatus, ResponseHeaders, ResponseBody, Req),
            {ok, Req2, State};

        [] ->
            io:format("App NOT FOUND ~n")
    end.

%% Extract request Values
request_dump(AppHost, AppPort, Req) ->
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

  RequestUrl = <<AppHost/binary, AppPort/binary, Path/binary, QueryString/binary>>,
  {Method, RequestUrl, Headers}.



%% Just to Test
setup_applications() ->
    Repository = ets:new(applications, [set, named_table, public]),
    ets:insert(Repository, {<<"localhost">>, <<":4567">>, active}),
    ets:insert(Repository, [ 
      {<<"www.myhost.com">>, <<":4567">>, active}, 
      {<<"www.hostinactive.com">>, <<":4568">>, inactive} 
    ]),
    Repository.

setup_proxies() ->
    Proxies = ets:new(proxies, [set, named_table, public]),
    Proxies.

