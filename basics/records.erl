-module(records).

-compile(export_all).

%% removes the compile error for exporting all the functions.
-compile(nowarn_export_all).

-include("something.hrl").

%% Records.
-record(user, {id, name, group, age}).

%% Records.
%% use pattern matching to filter.

first_user() ->
    USER = #user{name = "Zeus", group = admin, age = 1000},
    USER.

admin_panel(#user{name = Name, group = admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name = Name}) ->
    Name ++ " is not allowed".

allow_user(U) when U#user.age > 18 -> allowed;
allow_user(_) -> forbidden.

%% Empty record.
get_something() -> #random{}.
