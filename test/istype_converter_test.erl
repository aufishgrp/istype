-module(istype_converter_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

convert_test() ->
    Value = {atom, 1, atom},
    Type0 = {type, atom, []},
    Expected0 = {call, 1, {remote, 1, {atom, 1, istype_lib}, {atom, 1, totype}},
		 [Value, erl_parse:abstract(Type0, [{line, 1}]), {map, 1, []}, {map, 1, []}, {nil, 1}]},
    istype_test_util:match(istype_converter, Value, Expected0, Type0, []).
