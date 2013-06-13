-module(index_handler).

-behaviour(cowboy_http_handler).

%% Cowboy_http_handler callbacks
-export([
	init/3,
	handle/2,
	terminate/3
]).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	{ok, Body, Req2} = cowboy_req:body(Req),
	PostVals = jsonx:decode(Body, [{format, proplist}]),
	Login = proplists:get_value(<<"login">>, PostVals),
	AuthInfo = proplists:get_value(Login, ets:lookup(sessions, Login)),
	Reply = check(AuthInfo, PostVals),
	{ok, Req3} = cowboy_req:reply(200, [], Reply, Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% Internal methods
%% TODO сделать проверку на валидность запроса (запрос мог быть скопирован)
check(undefined, _) ->
	out:server_error(session_not_found);

check({ServerKey, Pid}, PostVals) ->
	ClientKey = proplists:get_value(<<"key">>, PostVals),
	check(ServerKey, ClientKey, Pid, PostVals).

check(Key, Key, Pid, PostVals) ->
	Cmd = proplists:get_value(<<"cmd">>, PostVals),
	execute_cmd(Pid, Cmd, PostVals);

check(_, _, _, _) ->
	out:server_error(incorrect_key).


execute_cmd(_Pid, <<"test">>, _PostVals) ->
	out:server_answer(<<"testing...">>);

execute_cmd(Pid, <<"sync">>, PostVals) ->
	Data = proplists:get_value(<<"data">>, PostVals),
	Msg = gen_server:call(Pid, {sync, Data}),
	out:server_answer(Msg);

execute_cmd(_Pid, <<"send_msg">>, PostVals) ->
	Msg = proplists:get_value(<<"msg">>, PostVals),
	To = proplists:get_value(<<"to">>, PostVals),
	From = proplists:get_value(<<"login">>, PostVals),
	user:send_msg(From, To, Msg),
	out:server_answer([{ok, no_data}]);

execute_cmd(Pid, <<"friend_add">>, PostVals) ->
	Login = proplists:get_value(<<"login">>, PostVals),
	FriendLogin = proplists:get_value(<<"friend_login">>, PostVals),
	Res = user:friend_add(Login, FriendLogin),
	case Res of
		{Save, Status} ->
			gen_server:call(Pid, {new_friend, Save}),
			out:server_answer([{status, Status}]);
		Error -> Error
	end;

execute_cmd(_Pid, <<"friend_list">>, PostVals) ->
	Login = proplists:get_value(<<"login">>, PostVals),
	user:friend_list(Login);

execute_cmd(Pid, <<"friend_delete">>, PostVals) ->
	Login = proplists:get_value(<<"login">>, PostVals),
	FriendLogin = proplists:get_value(<<"friend_login">>, PostVals),
	user:friend_delete(Login, FriendLogin),
	gen_server:call(Pid, {friend_delete, FriendLogin}),
	out:server_answer([{ok, no_data}]);

execute_cmd(_Pid, <<"stat">>, PostVals) ->
	Data = proplists:get_value(<<"data">>, PostVals),
	Login = proplists:get_value(<<"login">>, PostVals),
	stat_log:save(Login, Data),
	out:server_answer([{ok, no_data}]);

execute_cmd(_Pid, undefined, _PostVals) ->
	out:server_error(miss_cmd_param);

execute_cmd(_Pid, _Cmd, _PostVals) ->
	out:server_error(incorrect_cmd_param).