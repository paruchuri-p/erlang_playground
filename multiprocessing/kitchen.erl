-module(kitchen).

-compile(export_all).

%% removes the compile error for exporting all the functions.
-compile(nowarn_export_all).

%% 1> c(kitchen).
%% {ok,kitchen}
%% 2> Pid = kitchen:spawn_fridge([]).
%% <0.87.0>
%% 3> kitchen:get_item(Pid, "Milk").
%% item_not_found
%% 4> kitchen:put_item(Pid, "Milk").
%% ok
%% 5> kitchen:get_item(Pid, "Milk").
%% "Milk"
%% 6> kitchen:get_item(Pid, "Milk").
%% item_not_found

%% spawns the fridge process.
spawn_fridge(InitialItems) ->
    spawn(?MODULE, fridge, [InitialItems]).

%% Puts item in the fridge.
put_item(FridgeId, Item) ->
    FridgeId ! {self(), {put, Item}},
    receive {FridgeId, ok} -> ok after 5000 -> timeout end.

%% Gets Item from the fridge.
get_item(FridgeId, Item) ->
    FridgeId ! {self(), {get, Item}},
    receive
      {FridgeId, Item} -> Item;
      {FridgeId, _} -> item_not_found
      after 5000 -> timemout
    end.

fridge(Items) ->
    receive
      {Caller, {put, Item}} ->
	  Caller ! {self(), ok}, fridge([Item | Items]);
      {Caller, {get, Item}} ->
	  case lists:member(Item, Items) of
	    true ->
		Caller ! {self(), Item},
		fridge(lists:delete(Item, Items));
	    false ->
		Caller ! {self(), item_not_found}, fridge(Items)
	  end;
      terminate -> ok
    end.
