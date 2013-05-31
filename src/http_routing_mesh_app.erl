-module(http_routing_mesh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
      {'_', [{'_', http_routing_mesh, []}]}
    ]),
  cowboy:start_http(http_routing_mesh, 10,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),

  io:format("HTTP Router Started at port : 8080 ~n"),
  http_routing_mesh_sup:start_link().

stop(_State) ->
    ok.
