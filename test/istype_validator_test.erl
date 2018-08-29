-module(istype_validator_test).

-compile(export_all).

-include_lib("istype.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(BIF(Type, Bif),
	fun () ->
		Value = {atom, 1, atom},
		Expected = {call, 1, {atom, 1, Bif}, [Value]},
		istype_test_util:match(istype_validator, Value, Expected, #type{type = Type}, [])
	end()).

-define(REMOTE(Value, Type),
	fun () ->
		Abs = erl_parse:abstract(istype_transform:substitute_literals(Type), [{line, 1}]),
		Expected = {call, 1, {remote, 1, {atom, 1, istype_lib}, {atom, 1, istype}}, [Value, Abs, {map, 1, []}, {map, 1, []}, {nil, 1}]},
		istype_test_util:match(istype_validator, Value, Expected, Type, [])
	end()).

literal_test() ->
    Value = {atom, 1, atom},
    Literal = {atom, 1, also},
    Type = #literal{value = Literal},
    Expected = {op, 1, '=:=', Value, Literal},
    istype_test_util:match(istype_validator, Value, Expected, Type, []).

any_test() -> istype_test_util:match(istype_validator, {atom, 1, atom}, {atom, 1, true}, #type{type = any}, []).

none_test() -> istype_test_util:match(istype_validator, {atom, 1, atom}, {atom, 1, false}, #type{type = none}, []).

pid_test() -> ?BIF(pid, is_pid).

port_test() -> ?BIF(port, is_port).

reference_test() -> ?BIF(reference, is_reference).

atom_test() -> ?BIF(atom, is_atom).

'Bitstring_test'() ->
    Value = {atom, 1, atom},
    M0 = 0,
    N0 = 0,
    Type0 = #type{type = bitstring, spec = {M0, N0}},
    Expected0 = {op, 1, '=:=', Value, {bin, 1, []}},
    istype_test_util:match(istype_validator, Value, Expected0, Type0, []),
    M1 = 7,
    N1 = 0,
    Type1 = #type{type = bitstring, spec = {M1, N1}},
    Expected1 = {op, 1, 'andalso', {call, 1, {atom, 1, is_bitstring}, [Value]},
		 {op, 1, '=:=', {call, 1, {atom, 1, bit_size}, [Value]}, {integer, 1, M1}}},
    istype_test_util:match(istype_validator, Value, Expected1, Type1, []),
    M2 = 0,
    N2 = 7,
    Type2 = #type{type = bitstring, spec = {M2, N2}},
    Expected2 = {op, 1, 'andalso', {call, 1, {atom, 1, is_bitstring}, [Value]},
		 {op, 1, '=:=', {op, 1, 'rem', {call, 1, {atom, 1, bit_size}, [Value]}, {integer, 1, N2}}, {integer, 1, M2}}},
    istype_test_util:match(istype_validator, Value, Expected2, Type2, []),
    M3 = 7,
    N3 = 7,
    Type3 = #type{type = bitstring, spec = {M3, N3}},
    Expected3 = {op, 1, 'andalso', {call, 1, {atom, 1, is_bitstring}, [Value]},
		 {op, 1, '=:=', {op, 1, 'rem', {call, 1, {atom, 1, bit_size}, [Value]}, {integer, 1, N3}}, {integer, 1, M3}}},
    istype_test_util:match(istype_validator, Value, Expected3, Type3, []).

float_test() -> ?BIF(float, is_float).

integer_test() -> ?BIF(integer, is_integer).

'Integer..Integer_test'() ->
    Value = {atom, 1, atom},
    Type0 = #type{type = range, spec = {#literal{value = {integer, 1, 1}}, #literal{value = {integer, 1, 2}}}},
    Expected0 = {op, 1, 'andalso', {call, 1, {atom, 1, is_integer}, [Value]},
		 {op, 1, 'andalso', {op, 1, '>=', Value, {integer, 1, 1}}, {op, 1, '=<', Value, {integer, 1, 2}}}},
    istype_test_util:match(istype_validator, Value, Expected0, Type0, []),
    Type1 = #type{type = range, spec = {#literal{value = {integer, 1, 1}}, #literal{value = {atom, 1, undefined}}}},
    Expected1 = {op, 1, 'andalso', {call, 1, {atom, 1, is_integer}, [Value]}, {op, 1, '>=', Value, {integer, 1, 1}}},
    istype_test_util:match(istype_validator, Value, Expected1, Type1, []),
    Type2 = #type{type = range, spec = {#literal{value = {atom, 1, undefined}}, #literal{value = {integer, 1, 2}}}},
    Expected2 = {op, 1, 'andalso', {call, 1, {atom, 1, is_integer}, [Value]}, {op, 1, '=<', Value, {integer, 1, 2}}},
    istype_test_util:match(istype_validator, Value, Expected2, Type2, []).

'List_test'() ->
    Value = {atom, 1, atom},
    Type0 = #type{type = list, spec = {maybe_empty, #type{type = any}, #type{type = any}}},
    Expected0 = {call, 1, {atom, 1, is_list}, [Value]},
    istype_test_util:match(istype_validator, Value, Expected0, Type0, []),
    Type1 = #type{type = list, spec = {maybe_empty, #type{type = any}, #literal{value = {nil, 1}}}},
    Expected1 = {call, 1, {atom, 1, is_list}, [Value]},
    istype_test_util:match(istype_validator, Value, Expected1, Type1, []),
    Type2 = #type{type = list, spec = {nonempty, #type{type = any}, #type{type = any}}},
    Expected2 = {op, 1, 'andalso', {call, 1, {atom, 1, is_list}, [Value]}, {op, 1, '=/=', {nil, 1}, Value}},
    istype_test_util:match(istype_validator, Value, Expected2, Type2, []),
    Type3 = #type{type = list, spec = {nonempty, #type{type = any}, #literal{value = {nil, 1}}}},
    Expected3 = {op, 1, 'andalso', {call, 1, {atom, 1, is_list}, [Value]}, {op, 1, '=/=', {nil, 1}, Value}},
    istype_test_util:match(istype_validator, Value, Expected3, Type3, []),
    Type4 = #type{type = list, spec = {maybe_empty, #type{type = atom}, #type{type = any}}},
    ?REMOTE(Value, Type4).

'Map_test'() ->
    Value = {atom, 1, atom},
    Type0 = #type{type = map, spec = any},
    Expected0 = {call, 1, {atom, 1, is_map}, [Value]},
    istype_test_util:match(istype_validator, Value, Expected0, Type0, []),
    Type1 = #type{type = map, spec = empty},
    Expected1 = {op, 1, '=:=', Value, {map, 1, []}},
    istype_test_util:match(istype_validator, Value, Expected1, Type1, []),
    Type2 = #type{type = map, spec = {[{#literal{value = {atom, 1, key}}, #type{type = atom}}], []}},
    ?REMOTE(Value, Type2).

'Tuple_test'() ->
    Value = {atom, 1, atom},
    Type0 = #type{type = tuple, spec = any},
    Expected0 = {call, 1, {atom, 1, is_tuple}, [Value]},
    istype_test_util:match(istype_validator, Value, Expected0, Type0, []),
    Type1 = #type{type = tuple, spec = empty},
    Expected1 = {op, 1, '=:=', Value, {tuple, 1, []}},
    istype_test_util:match(istype_validator, Value, Expected1, Type1, []),
    Type2 = #type{type = tuple, spec = {1, [{1, #type{type = atom}}]}},
    Expected2 = {op, 1, 'andalso', {call, 1, {atom, 1, is_tuple}, [Value]},
		 {op, 1, 'andalso', {op, 1, '=:=', {integer, 1, 1}, {call, 1, {atom, 1, size}, [Value]}},
		  {call, 1, {atom, 1, is_atom}, [{call, 1, {atom, 1, element}, [{integer, 1, 1}, Value]}]}}},
    istype_test_util:match(istype_validator, Value, Expected2, Type2, []).

'Union_test'() ->
    Value = {atom, 1, atom},
    Type = #type{type = union, spec = [#type{type = atom}, #literal{value = {nil, 1}}]},
    Expected = {op, 1, 'orelse', {call, 1, {atom, 1, is_atom}, [Value]}, {op, 1, '=:=', Value, {nil, 1}}},
    istype_test_util:match(istype_validator, Value, Expected, Type, []).

record_test() ->
    Value = {atom, 1, atom},
    RecordTypes = #{a => #type{type = atom}, b => #type{type = atom},
		    c => #type{type = range, spec = {#literal{value = {integer, 1, 0}}, #literal{value = {integer, 1, 99}}}}},
    RecordDefaults = #{a => #literal{value = {nil, 1}}, b => #literal{value = {nil, 1}}, c => #literal{value = {nil, 1}}},
    RecordInfo = #record{record = record_a, arity = 3, fields = [a, b, c], types = RecordTypes, defaults = RecordDefaults},
    Records = #{record_a => RecordInfo},
    Type0 = #type{type = record, spec = {record_a, []}},
    Expected0 = {op, 1, 'andalso', {call, 1, {atom, 1, is_record}, [{atom, 1, atom}, {atom, 1, record_a}, {integer, 1, 3}]},
		 {op, 1, 'andalso', {call, 1, {atom, 1, is_atom}, [{record_field, 1, {atom, 1, atom}, record_a, {atom, 1, a}}]},
		  {op, 1, 'andalso', {call, 1, {atom, 1, is_atom}, [{record_field, 1, {atom, 1, atom}, record_a, {atom, 1, b}}]},
		   {op, 1, 'andalso', {call, 1, {atom, 1, is_integer}, [{record_field, 1, {atom, 1, atom}, record_a, {atom, 1, c}}]},
		    {op, 1, 'andalso', {op, 1, '>=', {record_field, 1, {atom, 1, atom}, record_a, {atom, 1, c}}, {integer, 1, 0}},
		     {op, 1, '=<', {record_field, 1, {atom, 1, atom}, record_a, {atom, 1, c}}, {integer, 1, 99}}}}}}},
    istype_test_util:match(istype_validator, Value, Expected0, Type0, #{}, Records, []).

custom_test() ->
    Value = {atom, 1, atom},
    Type0 = #type{module = ?MODULE, type = custom},
    Types = #{{?MODULE, custom, 0} => #type{type = atom}},
    Expected = {call, 1, {atom, 1, is_atom}, [Value]},
    Type1 = istype_parser:resolve_type(Type0, Types),
    istype_test_util:match(istype_validator, Value, Expected, Type1, Types, #{}, []).

parameterized_test() ->
    Value = {atom, 1, atom},
    TypeA = #type{type = tuple, spec = {1, [{1, {var, 1, 'A'}}]}, params = [{var, 1, 'A'}]},
    TypeB = #type{module = ?MODULE, type = type_a, spec = [{var, 1, 'B'}], params = [{var, 1, 'B'}]},
    TypeC = #type{module = ?MODULE, type = type_b, spec = [#type{type = atom}]},
    Types = #{{?MODULE, type_a, 1} => TypeA, {?MODULE, type_b, 1} => TypeB, {?MODULE, type_c, 0} => TypeC},
    Type0 = #type{module = ?MODULE, type = type_c},
    Type1 = istype_parser:resolve_type(Type0, Types),
    %%is_tuple(atom) andalso
    %%    1 =:= size(atom) andalso is_atom(element(1, atom))
    Expected0 = {op, 1, 'andalso', {call, 1, {atom, 1, is_tuple}, [Value]},
		 {op, 1, 'andalso', {op, 1, '=:=', {integer, 1, 1}, {call, 1, {atom, 1, size}, [Value]}},
		  {call, 1, {atom, 1, is_atom}, [{call, 1, {atom, 1, element}, [{integer, 1, 1}, Value]}]}}},
    istype_test_util:match(istype_validator, Value, Expected0, Type1, Types, #{}, []).
