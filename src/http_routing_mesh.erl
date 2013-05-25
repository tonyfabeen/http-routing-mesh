-module(http_routing_mesh).
-export([start/0, init/3, handle/2, terminate/3]).

-record(applications,{urls}).

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
  State = #applications{urls=setup_applications()},
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

    case orddict:find(Host, State#applications.urls) of
        {ok,AppStatus} ->
            io:format("App is ~p~n", AppStatus),
            http_proxy:start(Host, 8081);
        error ->
            io:format("App NOT FOUND ~n")
    end,

    Host1 = binary_to_list(Host),
    Response = ["<h3> Requested Host : ", Host1, " </h3>"],
    io:format("Generated Response ~p~n", [Response]),
    {ok, Req2} = cowboy_req:reply(200, [], Response, Req),
    {ok, Req2, State}.

%% Just to Test
setup_applications() ->
    A1 = orddict:new(),
    A2 = orddict:append(<<"localhost">>, active, A1),
    A3 = orddict:append(<<"www.myhost.com">>, active, A2),
    A4 = orddict:append(<<"www.hostinactive.com">>, inactive, A3),
    A4.

