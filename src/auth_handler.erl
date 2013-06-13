-module(auth_handler).

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
	Cmd = proplists:get_value(<<"cmd">>, PostVals),
	Reply = execute_cmd(Cmd, PostVals),
	{ok, Req3} = cowboy_req:reply(200, [], Reply, Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.


%% Internal methods

execute_cmd(<<"register">>, PostVals) ->
	user:register(PostVals);

execute_cmd(<<"auth">>, PostVals) ->
	Login = proplists:get_value(<<"login">>, PostVals),
	Password = proplists:get_value(<<"password">>, PostVals),
	user:auth(Login, Password);

execute_cmd(undefined, _PostVals) ->
	out:server_error(miss_cmd_param);

execute_cmd(_Cmd, _PostVals) ->
	out:server_error(incorrect_cmd_param).


