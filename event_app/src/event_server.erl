-module(event_server).

-compile(export_all).

-compile(nowarn_export_all).

-record(state, {events, clients}).

-record(event,
	{name = "", description = "", pid,
	 timeout = {{1970, 1, 1}, {0, 0, 0}}}).

init() ->
    loop(#state{events = orddict:new(),
		clients = orddict:new()}).

loop(S = #state{}) ->
    receive
      %% Client subscribe to Event Server.
      {Pid, MsgRef, {subscribe, Client}} ->
	  Ref = erlang:monitor(process, Client),
	  NewClients = orddict:store(Ref, Client,
				     S#state.clients),
	  Pid ! {MsgRef, ok},
	  loop(S#state{clients = NewClients});
      %% Client adding an event.
      {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
	  case valid_datetime(TimeOut) of
	    true ->
		EventPid = event:start_link(Name, TimeOut),
		NewEvents = orddict:store(Name,
					  #event{name = Name,
						 description = Description,
						 pid = EventPid,
						 timeout = TimeOut},
					  S#state.events),
		Pid ! {MsgRef, ok},
		loop(S#state{events = NewEvents});
	    false -> Pid ! {MsgRef, {error, bad_timeout}}, loop(S)
	  end
    end.

valid_datetime({Date, Time}) ->
    try calendar:valid_date(Date) andalso
	  calendar:valid_time(Time)
    catch
      error:function_clause -> false
    end;
valid_datetime(_) -> false.

valid_time({H, M, S}) -> valid_time(H, M, S).

valid_time(H, M, S)
    when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 ->
    true;
valid_time(_, _, _) -> false.
