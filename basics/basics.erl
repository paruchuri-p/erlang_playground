-module(basics).

-compile(export_all).

%% removes the compile error for exporting all the functions.
-compile(nowarn_export_all).

hello() -> io:format("Hello World").

add(A) -> A + 2.

greetAndAdd(X) -> hello(), add(X).

greet(male, Name) -> io:format('Hello Mr. ~s', [Name]);
greet(female, Name) ->
    io:format('Hello Ms. ~s', [Name]);
greet(_, Name) -> io:format('Hello ~s', [Name]).
