-module(out).

-define(format, json).

%% API
-export([
	server_answer/1,
	server_error/1
]).

%% Normal answer
server_answer(Data) ->
	server_answer(Data, ?format).

server_answer(Data, json)->
    jsonx:encode({Data}).

%% Business logic errors
server_error(miss_cmd_param) ->
	err(01);

server_error(incorrect_cmd_param) ->
	err(02);

server_error(user_already_exists) ->
	err(03);

server_error(user_not_exists) ->
	err(04);

server_error(password_not_valid) ->
	err(05);

server_error(session_not_found) ->
	err(06);

server_error(incorrect_key) ->
	err(07);

server_error(user_friend_not_exists) ->
	err(08);

%% undefined error
server_error(_Error) ->
	err(00).

err(Code) ->
	err(Code, ?format).

err(Code, json) ->
	jsonx:encode([{error, Code}]).
