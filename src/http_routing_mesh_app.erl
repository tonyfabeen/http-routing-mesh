-module(http_routing_mesh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, AppPort} = application:get_env(http_routing_mesh, port),
  {ok, AppAcceptors} = application:get_env(http_routing_mesh, acceptors),

  Dispatch = cowboy_router:compile([
      {'_', [{'_', http_routing_mesh, []}]}
    ]),
  cowboy:start_http(http_routing_mesh, AppAcceptors,
    [{port, AppPort}],
    [{env, [{dispatch, Dispatch}]}]
  ),

  io:format("HTTP Router Started at port : 8080 ~n"),
  http_routing_mesh_sup:start_link().

stop(_State) ->
    ok.
