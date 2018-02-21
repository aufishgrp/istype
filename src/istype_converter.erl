-module(istype_converter).

-export([transform/5, transform/7]).

%%====================================================================
%% transform functions
%%====================================================================
%% transform
%%==========================================================
transform(Module, Line, Value, Type, Options) ->
    transform(Module, Line, Value, Type, #{}, #{}, Options).

-spec transform(module(), integer(), istype:form(), istype:form(), istype:types(), istype:records(), istype:options()) -> istype:form().
%% @doc
%% @end
transform(_, Line, Value, Type, Types, Records, Options) ->
    {call, Line,
        {remote, Line,
            {atom, Line, istype_lib},
            {atom, Line, totype}},
        [Value,
         erl_parse:abstract(Type, [{line, Line}]),
         erl_parse:abstract(Types, [{line, Line}]),
         erl_parse:abstract(Records, [{line, Line}]),
         erl_parse:abstract(Options, [{line, Line}])]}.