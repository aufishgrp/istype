-module(istype_test_util).

-export([match/5, match/7]).

match(Module, Value, Type, Expected, Options) ->
    match(Module, Value, Type, Expected, #{}, #{}, Options).

match(Module, Value, Type, Expected, Types, Records, Options) ->
    Result = Module:transform(Value, Type, Types, Records, Options),
    try
        Expected = Result
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