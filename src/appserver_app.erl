-module(appserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([dispatch_rules/0]).

%% ===================================================================
%% API functions
%% ===================================================================

dispatch_rules() ->
	cowboy_router:compile([
		{'_', [
			{"/", index_handler, []},
			{"/auth", auth_handler, []},
			{'_', notfound_handler, []}
		]}
	]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dispatch = dispatch_rules(),
	Port = 8008,
	{ok, _} = cowboy:start_http(http_listener, 100,
		[{port, Port}],
		[{env, [{dispatch, Dispatch}]}]
	),
	sessions = ets:new(sessions, [set, named_table, public]),
	appserver_sup:start_link().

stop(_State) ->
    ok.
