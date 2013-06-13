-module(debug).

%% API
-export([print/1, print/2]).

print(Var) ->
	io:format("~n~p ~n", [Var]).

print(Name, [H|T]) ->
	io:format("~n ~p -> ~n~p, ~n", [Name, H]),
	print_list(T);
print(Name, Value) ->
    io:format("~n ~p -> ~n~p ~n", [Name, Value]).

print_list([H|[]]) ->
	io:format("~p. ~n",[H]);
print_list([H|T]) ->
	io:format("~p, ~n",[H]),
	print_list(T);
print_list([]) ->
    ok.