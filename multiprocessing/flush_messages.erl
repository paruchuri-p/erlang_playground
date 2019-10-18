-module(flush_messages).

-export([flush/0, priority_messages/0]).

%% Flushes out all the messages received.
flush() ->
    receive
      Message -> [Message | flush()] after 0 -> []
    end.

%% 1> c(flush_messages).
%% {ok,flush_messages}
%% 2> self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}.
%% {17,high}
%% 3> flush_messages:priority_messages().
%% [{15,high},{17,high},{7,normal},{1,normal}]

%% Flushes out the priority methods first and then the other messages.
priority_messages() ->
    receive
      {Message, high} ->
	  [{Message, high} | priority_messages()]
      after 0 -> normal_messages()
    end.

%% Low priority messages.
normal_messages() ->
    receive
      {Message, _} -> [{Message, normal} | normal_messages()]
      after 0 -> []
    end.
