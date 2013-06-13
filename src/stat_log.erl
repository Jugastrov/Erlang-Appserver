-module(stat_log).

%% API
-export([save/2]).

%% Stat database
-define(stat_db, stat).

%% Stat table
-define(stat_table, stat).

save(Login, Data) ->
	db:save(?stat_db, ?stat_table, {user_id, Login, data, Data}).

