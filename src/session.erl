-module(session).

-behavior(gen_server).

-export([start/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API methods
start(State) ->
	gen_server:start_link(session, State, []).

stop() ->
	gen_server:cast(session, stop).

%% API gen_server
init(State) ->
	{ok, State}.

handle_call({sync, Data}, _From, State) ->
	NewState = user:sync_state(State, Data),
	Reply = user:get_new_msgs(State),
	{reply, Reply, NewState};

handle_call({msg, Msg, Sender}, _From, State) ->
	Msgs = proplists:get_value(unread_msgs, State),
	NewMsgs = user:add_value(Msgs, {Sender, Msg}),
	NewState = proplists:delete(unread_msgs, State),
	{reply, [], NewState ++ [{unread_msgs, NewMsgs}]};

handle_call({new_friend, Friend}, _From, State) ->
	Friends = proplists:get_value(friends, State),
	NewFriends = user:add_value(Friends, Friend),
	NewState = proplists:delete(friends, State),
	{reply, [], NewState ++ [{friends, NewFriends}]};

handle_call(Msg, From, State) ->
	error_logger:error_msg("handle_call: unknown msg ~p from ~p ~n", [Msg, From]),
	{noreply, State}.

handle_cast(stop, State) ->
	io:format("handle_cast: normal stop ~n"),
	{stop, normal, State};

handle_cast(Msg, State) ->
	error_logger:error_msg("handle_cast: unknown msg ~p ~n", [Msg]),
	{noreply, State}.

handle_info(Msg, State) ->
	error_logger:error_msg("handle_info: unknown msg ~p ~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _extra) ->
	{ok, State}.

