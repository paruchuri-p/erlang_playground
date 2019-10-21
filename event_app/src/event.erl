-module(event).

-compile(export_all).

-compile(nowarn_export_all).

-record(state, {server, name = "", time_to_go = 0}).

%% Start event without linking.
start(Name, DateTime) ->
    spawn(?MODULE, init, [self(), Name, DateTime]).

%% Start event with a link.
start_link(Name, DateTime) ->
    spawn_link(?MODULE, init, [self(), Name, DateTime]).

%% Initializing the loop with normalized time.
init(Server, Name, DateTime) ->
    loop(#state{server = Server, name = Name,
		time_to_go = to_go(DateTime)}).

loop(S = #state{server = Server,
		time_to_go = [T | Next]}) ->
    receive
      {Server, Ref, cancel} -> Server ! {Ref, ok}
      after T * 1000 ->
		case Next of
		  [] -> Server ! {done, S#state.name};
		  _ -> loop(S#state{time_to_go = Next})
		end
    end.

%% Cancel the event.
cancel(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
      {Ref, ok} -> erlang:demonitor(Ref, [flush]), ok;
      {'DOWN', Ref, process, Pid, _Reason} -> ok
    end.

%% Erlang has a limit of 50 days. So we normalize it.
normalize(N) ->
    Limit = 49 * 24 * 60 * 60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].

%% The input is given in the date time format. We convert it to seconds.
to_go(TimeOut = {{_, _, _}, {_, _, _}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
	     calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 -> ToGo;
	      ToGo =< 0 -> 0
	   end,
    normalize(Secs).

normalized_loop(S = #state{time_to_go = N}) ->
    loop(S#state{time_to_go = normalize(N)}).
