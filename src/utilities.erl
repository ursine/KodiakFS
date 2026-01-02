-module(utilities).

-export([timestamp/0]).

timestamp() ->
    {Megasec, Sec, Millis} = erlang:timestamp(),
    Megasec*1000000 + Sec + Millis/1000.0.


