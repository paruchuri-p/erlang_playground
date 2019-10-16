-module(sleep_timer).

-export([sleep/1]).

%% sleeps for T secs.
sleep(T) -> receive  after T * 1000 -> awake end.
