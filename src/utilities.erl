-module(utilities).

-export([timestamp/0]).

timestamp() ->
    {Megasec, Sec, Millis} = erlang:timestamp(),
    Megasec * 1000000000 + Sec * 1000 + Millis.


