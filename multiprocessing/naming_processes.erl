-module(naming_processes).

-compile(export_all).

-compile(nowarn_export_all).

spawn_critic() -> spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic1, []),
    register(critic, Pid),
    receive
      {'EXIT', Pid, normal} -> % not a crash
	  ok;
      {'EXIT', Pid,
       shutdown} -> % manual termination, not a crash
	  ok;
      {'EXIT', Pid, _} -> restarter()
    end.

review(critic, Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    %% One way to do it, but race condition would drown this code. Instead use Ref.
    % Pid = whereis(critic),
    receive
      {Ref, Review} -> Review after 1000 -> timeout
    end.

critic1() ->
    receive
      {Caller, Ref,
       {"Rage Against the Turing Machine", "Unit Testify"}} ->
	  Caller ! {Ref, "They are great!"};
      {Caller, Ref, {"System of a Downtime", "Memoize"}} ->
	  Caller !
	    {Ref,
	     "They're not Johnny Crash but they're "
	     "good."};
      {Caller, Ref,
       {"Johnny Crash", "The Token Ring of Fire"}} ->
	  Caller ! {Ref, "Simply incredible."};
      {Caller, Ref, {_Band, _Album}} ->
	  Caller ! {Ref, "They are terrible!"}
    end,
    critic1().
