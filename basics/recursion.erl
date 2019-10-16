-module(recursion).

% -export([add/1, dupl/2, factorial/1, greet/2,
% 	 greetAndAdd/1, hello/0, len/1, quicksort/1, reverse/1,
% 	 sublist/2, zip/2]).

-compile(export_all).

%% removes the compile error for exporting all the functions.
-compile(nowarn_export_all).

-author('Poseidon').

%% Factorial.

% factorial(1) -> 1;
% factorial(N) when N > 1 -> N * factorial(N - 1).

%% Facrtorial - Tail recursion.
factorial(N) -> factorial(N, 1).

factorial(0, Acc) -> 1 * Acc;
factorial(N, Acc) -> factorial(N - 1, Acc * N).

%% Length of an array.

% len([]) -> 0;
% len([_ | T]) -> 1 + len(T).

%% Length of an array - Tail Recursion.
len(T) -> len(T, 0).

len([], Acc) -> 0 + Acc;
len([_ | T], Acc) -> len(T, Acc + 1).

%% Duplicate the value Num of times.

dupl(N, Num) -> dupl(N, Num, []).

dupl(_, 0, F) -> F;
dupl(N, Num, F) -> dupl(N, Num - 1, [N | F]).

%% Reverse an array.

reverse(A) -> reverse(A, []).

reverse([], Acc) -> Acc;
reverse([H | T], Acc) -> reverse(T, [H | Acc]).

%% Get sublist of size Num.

sublist(A, Num) -> sublist(A, Num, []).

sublist(_, 0, Acc) -> reverse(Acc);
sublist([], _, Acc) -> reverse(Acc);
sublist([H | T], Num, Acc) ->
    sublist(T, Num - 1, [H | Acc]).

%% Merge two arrays.

zip(X, Y) -> zip(X, Y, []).

zip([], _, Acc) -> reverse(Acc);
zip(_, [], Acc) -> reverse(Acc);
zip([X | Xs], [Y | Ys], Acc) ->
    zip(Xs, Ys, [{X, Y} | Acc]).

%% Quick sort.

partition(Pivot, Rest) ->
    partition(Pivot, Rest, [], []).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H | T], Smaller, Larger)
    when Pivot < H ->
    partition(Pivot, T, Smaller, [H | Larger]);
partition(Pivot, [H | T], Smaller, Larger)
    when Pivot >= H ->
    partition(Pivot, T, [H | Smaller], Larger).

quicksort([]) -> [];
quicksort([Pivot | Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

%% Higher Order Function.

two() -> 2.

one() -> 1.

addFuncs(X, Y) -> X() + Y().

%% find the maximum of a list
max([H | T]) -> max2(T, H).

max2([], Max) -> Max;
max2([H | T], Max) when H > Max -> max2(T, H);
max2([_ | T], Max) -> max2(T, Max).

%% find the minimum of a list
min([H | T]) -> min2(T, H).

min2([], Min) -> Min;
min2([H | T], Min) when H < Min -> min2(T, H);
min2([_ | T], Min) -> min2(T, Min).

%% sum of all the elements of a list
sum(L) -> sum(L, 0).

sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, H + Sum).

%% reducing the above three to one func. -FOLD
fold(_, [], Acc) -> Acc;
fold(F, [H | T], Acc) -> fold(F, T, F(H, Acc)).

%% [10, 4, 3, "+", 2, "*", "-"]

calculate([O1, O2 | T], "*") -> [O1 * O2 | T];
calculate([O1, O2 | T], "-") -> [O2 - O1 | T];
calculate([O1, O2 | T], "+") -> [O1 + O2 | T];
calculate([O1, O2 | T], "/") -> [O2 / O1 | T];
calculate(Acc, H) -> [H | Acc].

calc(A) -> calc(A, []).

calc([], [A]) -> A;
calc([H | T], Acc) -> calc(T, calculate(Acc, H)).

%% Test
test() -> A = {hello, world, 1}, A.

%% Heathrow to London.
convert_to_tuples([], Acc) -> lists:reverse(Acc);
convert_to_tuples([A, B, X | T], Acc) ->
    convert_to_tuples(T, [{A, B, X} | Acc]).

calculate_path() ->
    case file:read_file("road.txt") of
      {ok, Road} ->
	  Path_list = string:tokens(binary_to_list(Road),
				    "\n\t\r"),
	  Path_list_int = lists:map(fun (X) -> list_to_integer(X)
				    end,
				    Path_list),
	  Path_tuples = convert_to_tuples(Path_list_int, []),
	  Path_tuples;
      {error, _} -> "Error reading the file."
    end.
