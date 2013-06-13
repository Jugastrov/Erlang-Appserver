-module(db).

%% API
-export([
	save/3,
	save/4,
	get/3,
	get/4,
	list/1,
	delete/1
]).


%% Insert
save(DB, Collection, Tuple) ->
	mongo:do(safe, master, db_con, DB, fun() ->
		mongo:insert(Collection, Tuple)
	end).

%% Update
save(DB, Collection, Selector, Doc) ->
	mongo:do(safe, master, db_con, DB, fun() ->
		mongo:update(Collection, Selector, Doc)
	end).

%% Get one (selected fields)
get(DB, Collection, Tuple, Fields) ->
	to_proplist(mongo:do(safe, master, db_con, DB, fun() ->
		mongo:find_one(Collection, Tuple, Fields)
	end)).

%% Get one (all fields)
get(DB, Collection, Tuple) ->
	to_proplist(mongo:do(safe, master, db_con, DB, fun() ->
		mongo:find_one(Collection, Tuple)
	end)).

%% Get list 
list(_Tuple) ->
	ok.

%% Delete 
delete(_Tuple) ->
	ok.

%find(Collection, Selector) ->
%	find(Collection, Selector, []).

%find(Collection, Selector, Projector) ->
%	Cursor = mongo:find(Collection, Selector, Projector),
%	Result = mongo_cursor:rest(Cursor),
%	mongo_cursor:close(Cursor),
%	Result.

%% Convert Mongo tuple to proplist
to_proplist([]) ->
	[];
to_proplist({}) ->
	[];
to_proplist({Tuple}) ->
	to_proplist(Tuple);
to_proplist(Tuple) ->
	Size = tuple_size(Tuple),
	[{element(X, Tuple), element(X + 1, Tuple)} || X <- lists:seq(1, Size, 2)].

