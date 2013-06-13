-module(user).

%% API
-export([
	register/1,
	auth/2,
	sync_state/2,
	friend_add/2,
	friend_list/1,
	friend_delete/2,
	send_msg/3,
	get_new_msgs/1,
	add_value/2
]).

%% User database
-define(users_db, users).

%% User tables
-define(users_table, users).
-define(friends_table, user_friends).

%% Friendship statuses
-define(status_friend_request, 1).
-define(status_friendship_complite, 2).

%% TODO добавить проверку логина и пароля на корректность
register(RegInfo) ->
	Login = proplists:get_value(<<"login">>, RegInfo),
	Password = proplists:get_value(<<"password">>, RegInfo),
	User = db:get(?users_db, ?users_table, {'_id', Login}),
	case User of
		[] ->
			Data = proplists:get_value(<<"data">>, RegInfo),
			State = save_user_info(Login, Password, Data),
			start_session(Login, State);
		_ ->
			out:server_error(user_already_exists)
	end.

auth(Login, UserPassword) ->
	User = db:get(?users_db, ?users_table, {'_id', Login}),
	case User of
		[] ->
			out:server_error(user_not_exists);
		_ ->
			ServerPassword = proplists:get_value(password, User),
			auth(Login, User, {UserPassword, ServerPassword})
	end.

auth(Login, State, {Password, Password}) ->
	start_session(Login, State);

auth(_, _, _) ->
	out:server_error(password_not_valid).

sync_state(State, Data) ->
	NewState = [sync_field({Key, Value}, get_value(Key, Data, atom)) || {Key, Value} <- State],
	NewState.

friend_add(Login, FriendLogin) ->
	User = db:get(?users_db, ?users_table, {'_id', Login}),
	case User of
		undefined ->
			out:server_error(user_not_exists);
		_ ->
			save_user_friend(Login, FriendLogin)
	end.

friend_list(Login) ->
	db:list(?users_db, ?friends_table, {user_2, Login}).

friend_delete(Login, FriendLogin) ->
	db:delete(?users_db, ?friends_table, {user_1, Login, user_2, FriendLogin}),
	db:delete(?users_db, ?friends_table, {user_1, FriendLogin, user_2, Login}).

%% TODO научиться посылать не только на локальном узле
send_msg(From, To, Msg) ->
	AuthInfo = proplists:get_value(To, ets:lookup(sessions, To)),
	case AuthInfo of
		undefined ->
			send_offline(From, To, Msg);
		{_Key, Pid} ->
			gen_server:call(Pid, {msg, Msg, From})
	end.

get_new_msgs(State) ->
	check_msg(proplists:get_value(unread_msgs, State)).

add_value(undefined, Value) ->
	[Value];

add_value(List, Value) ->
	List ++ [Value].

%% ===================================================================
%% Internal methods
%% ===================================================================

save_user_info(Login, Password, Data) ->
	Age = proplists:get_value(<<"age">>, Data),
	OS = proplists:get_value(<<"os">>, Data),
	db:save(?users_db, ?users_table, {'_id', Login, password, Password, age, Age, os, OS}),
	Data ++ [Login].

%% TODO для каждого поля создать клаузу проверяющую возможность изменения
sync_field(Field, undefined) ->
	Field;
sync_field({Key, _Value}, NewValue) ->
	{Key, NewValue}.

get_value(Key, From, binary) ->
	proplists:get_value(atom_to_binary(Key, utf8), From);
get_value(Key, From, atom) ->
	proplists:get_value(Key, From).

send_offline(From, To, Msg) ->
	Msgs = db:get(?users_db, ?users_table, {'_id', To}, {'_id', 0, unread_msgs, 1}),
    UnreadMsgs = proplists:get_value(unread_msgs, Msgs),
	NewMsgs = user:add_value(UnreadMsgs, {From, Msg}),
	db:save(?users_db, ?users_table, {'_id', To}, {'$set', {unread_msgs, NewMsgs}}),
	ok.

check_msg(undefined) ->
	[{ok, no_data}];

check_msg(NewMsgs) ->
	[{new_msgs, NewMsgs}].

save_user_friend(Login, FriendLogin) ->
	Friendship = db:get(?users_db, ?friends_table, {user_1, FriendLogin}),
	debug:print("Friendship", Friendship),
	case Friendship of
		[] ->
			Save = db:save(?users_db, ?friends_table, {user_1, Login, user_2, FriendLogin, status, ?status_friend_request}),
			{Save, ?status_friend_request};
		[_User1, _User2, {status, ?status_friend_request}, {'_id', Id}] ->
			Save = db:save(?users_db, ?friends_table, {'_id', Id}, {'$set', {status, ?status_friendship_complite}}),
			{Save, ?status_friendship_complite}
	end.

%% TODO номер узла задавать в конфиге или автоматически
start_session(Login, State) ->
	{ok, Pid} = session:start(State),
	Key = random:uniform(10000), %% генерирует не правильно
	true = ets:insert(sessions, {Login, {Key, Pid}}),
	out:server_answer([{node_num, 1}, {key, Key}]).

