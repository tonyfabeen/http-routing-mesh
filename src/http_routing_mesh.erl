-module(http_routing_mesh).
-export([start/0, init/3, handle/2, terminate/3]).

-record(state, {http_client, eredis}).

start() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowboy),
  application:start(http_routing_mesh).

%% =======================================
%% Cowboy Callbacks
%% =======================================
init({tcp, http}, Req, _Opts) ->
  {ok, Client} = cowboy_client:init([]),
  {ok, ERedis}  = eredis:start_link(),
  State = #state{http_client=Client, eredis=ERedis},
  {ok, Req, State}.

%%
handle(Req, State) ->

  {Host, Req1} = cowboy_req:host(Req),

  case find_app_for(Host, State) of

    {AppInstances, AppUrl} when AppInstances >= 1 ->
        delegate_request_to_app(Req1, AppUrl, State);
    _ ->
        {ok, Req2 }   = cowboy_req:reply(404, [], "<h1>Sorry, App Not Found</h1>", Req1),
        {ok, Req2, State}

  end.

%%
terminate(_Reason, _Req, _State) ->
  ok.


%% =======================================
%% Private Functions
%% =======================================

%% Find Application
find_app_for(Host,State) ->
  AppInstances = app_instances_for(Host, State),
  AppUrl = app_url_for(AppInstances, Host, State),
  {AppInstances, AppUrl}.

%%Return Nr of Instances of an APP
app_instances_for(Host, State) -> 
  {_, NrInstances} = eredis:q(State#state.eredis, ["LLEN", Host]),
  AppInstances = list_to_integer(binary_to_list(NrInstances)),
  AppInstances.

%% 
app_url_for(0, _Host, _State) -> <<"">>;
app_url_for(AppInstances, Host, State) ->
  {_, AppUrls} = eredis:q(State#state.eredis, ["LRANGE", <<"http:",Host/binary>>, "0", "-1"]),
  AppUrl = lists:nth(random:uniform(AppInstances), AppUrls),
  AppUrl.


%% Delegate a Request to an application
delegate_request_to_app(Req, AppUrl, State) ->

  %% Send Request to App
  {Method, RequestUrl, Headers} = request_dump(AppUrl, Req),
  {ok, Client2} = cowboy_client:request(Method, RequestUrl, Headers, State#state.http_client),
  {ok, ResponseStatus, ResponseHeaders, Client3} = cowboy_client:response(Client2),
  {ok, ResponseBody, _Client4} = cowboy_client:stream_body(Client3),

  %% Send Response Client
  {ok, Req2 }   = cowboy_req:reply(ResponseStatus, ResponseHeaders, ResponseBody, Req),
  {ok, Req2, State}.


%% Extract request Values and Build Request Url to Send to a Backend
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

  RequestUrl = <<AppUrl/binary, Path/binary, QueryString/binary>>,
  {Method, RequestUrl, Headers}.

