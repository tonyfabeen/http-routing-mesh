-module(http_routing_mesh).
-export([start/0, init/3, handle/2, terminate/3]).

-record(applications,{repository}).

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
  State = #applications{repository=setup_applications()},
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
    case ets:lookup(State#applications.repository, Host) of
        [App] ->
            {AppHost, AppPort, AppStatus} = App,
            io:format("App is registered for => URL :  ~p PORT: ~p STATUS: ~p ~n", [AppHost, AppPort, AppStatus]),
            http_proxy:start(AppHost, 8081);
        [] ->
            io:format("App NOT FOUND ~n")
    end,

    %% Should get the Http Proxy Pid, associate to a App host
    %% and when it there is some instance responding to That, just delegate the request
    %% something like that :
    %% HttpProxyPid ! {self(), Req, State}
    %% and here:
    %% receive
    %%   {From, Response, ... } ->
    %%      {ok, Req2} = cowboy_req:reply(200, [], Response, Req),
    %%      {ok, Req2, State}.
    %% end

    Host1 = binary_to_list(Host),
    Response = ["<h3> Requested Host : ", Host1, " </h3>"],
    io:format("Generated Response ~p~n", [Response]),
    {ok, Req2} = cowboy_req:reply(200, [], Response, Req),
    {ok, Req2, State}.

%% Just to Test
setup_applications() ->
    Repository = ets:new(applications, [set, named_table, public]),
    ets:insert(Repository, {<<"localhost">>, 4567, active}),
    ets:insert(Repository, [ {<<"www.myhost.com">>, 4567, active}, {<<"www.hostinactive.com">>, 4567, inactive} ]),
    Repository.

