-module(appserver).

%% API
-export([
	start/0,
	stop/0,
	update_routes/0
]).

-define(APPS, [crypto, ranch, cowboy, bson, mongodb, appserver]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	ok = ensure_started(?APPS),
	ok = sync:go(),
	{ok, Pid} = mongo_connection:start_link({"127.0.0.1", 27017}, []),
	register(db_con, Pid).

stop() ->
	sync:stop(),
	ok = stop_apps(lists:reverse(?APPS)).

update_routes() ->
	Routes = appserver_app:dispatch_rules(),
	cowboy:set_env(http_listener, dispatch, Routes).


%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	application:stop(App),
	stop_apps(Apps).