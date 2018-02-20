-module(istype_test_util).

-include_lib("eunit/include/eunit.hrl").

-export([match/5, match/7]).

match(Module, Value, Expected, Type, Options) ->
    match(Module, Value, Expected, Type, #{}, #{}, Options).

match(Module, Value, Expected, Type, Types, Records, Options) ->
    Line = element(2, Value),
    Result = Module:transform(Module, Line, Value, Type, Types, Records, Options),
    try
        ?assertEqual(Expected, Result)
    catch
        _:_ ->
            io:format("\n++++++++++++++++", []),
            io:format("\nExpected:", []),
            io:format("\n~p", [Expected]),
            io:format("\n================", []),
            io:format("\nResult:", []),
            io:format("\n~p", [Result]),
            io:format("\n================", []),
            io:format("\nHuman:", []),
            io:format("\n~s", [forms:from_abstract(Result)]),
            io:format("\n----------------\n", []),
            Expected = Result
    end.