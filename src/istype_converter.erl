-module(istype_converter).

-export([transform/4, transform/6]).

%%====================================================================
%% transform functions
%%====================================================================
%% transform
%%==========================================================
transform(Line, Value, Type, Options) ->
    transform(Line, Value, Type, #{}, #{}, Options).

-spec transform(integer(), istype:form(), istype:form(), istype:types(), istype:records(), istype:options()) -> istype:form().
%% @doc
%% @end
transform(Line, Value, Type, Types, Records, Options) ->
    {call, Line,
        {remote, Line,
            {atom, Line, istype_lib},
            {atom, Line, totype}},
        [Value,
         erl_parse:abstract(Type, [{line, Line}]),
         erl_parse:abstract(Types, [{line, Line}]),
         erl_parse:abstract(Records, [{line, Line}]),
         erl_parse:abstract(Options, [{line, Line}])]}.