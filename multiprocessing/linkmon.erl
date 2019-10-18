-module(linkmon).

-compile(export_all).

-compile(nowarn_export_all).

%% Shell spawns and links to the chain.
spawn_chain(N) ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, chain, [N]).

%% Shell spawns and monitors the chain.
% 1> c(linkmon).
% {ok,linkmon}
% 2> linkmon:monitor_chain(3).
% {<0.85.0>,#Ref<0.2599163239.1348468743.92179>}
% 3> flush().
% Shell got {'DOWN',#Ref<0.2599163239.1348468743.92179>,process,<0.85.0>,
%                   "chain dies here"}
monitor_chain(N) -> spawn_monitor(?MODULE, chain, [N]).

%% Once the last member of the chain exits, every other member exits because of the link.
chain(0) ->
    receive
      _ -> ok after 1000 -> exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun () -> chain(N - 1) end),
    link(Pid),
    receive _ -> ok end.
