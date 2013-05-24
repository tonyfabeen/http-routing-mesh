-module(http_routing_mesh).
-export([start/0, init/3, handle/2, terminate/3]).

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
  ).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Host, Req1} = cowboy_req:host(Req),
  Host1 = binary_to_list(Host),
  Response = ["<h3> Requested Host : ", Host1, " </h3>"],
  io:format("Generated Response ~p~n", [Response]),
  {ok, Req2} = cowboy_req:reply(200, [], Response, Req1),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.