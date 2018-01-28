-module(istype_converter).

-export([transform/3, transform/5]).

%%====================================================================
%% transform functions
%%====================================================================
%% transform
%%==========================================================
transform(Value, Type, Options) ->
    transform(Value, Type, #{}, #{}, Options).

-spec transform(Value :: istype:form(), Type :: istype:form(), istype:types(), istype:records(), istype:options()) -> istype:form().
%% @doc 
%% @end
transform(Value, Type, Types, Records, Options) ->
    Line = element(2, Value),
    {call, Line,
        {remote, Line,
            {atom, Line, istype_lib},
            {atom, Line, totype}},
        [Value,
         erl_parse:abstract(Type, [{line, Line}]),
         erl_parse:abstract(Types, [{line, Line}]),
         erl_parse:abstract(Records, [{line, Line}]),
         erl_parse:abstract(Options, [{line, Line}])]}.