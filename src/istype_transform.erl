-module(istype_transform).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
	UserTypes = forms:reduce(fun get_types/2, #{}, Forms),
	forms:map(fun(Form) -> do_transform(Form, UserTypes) end, Forms).

get_types({attribute, _, type, {Type, TypeInfo, []}}, Acc) ->
	Acc#{Type => TypeInfo};
get_types(_, Acc) -> Acc.

do_transform({call, Line, {atom, _, istype}, [{var, _, Var}, {call, _, {atom, _, Type}, TypeArgs}]}, UserTypes) ->
	istype_to_gaurd(Line, Var, {type, Line, Type, TypeArgs}, UserTypes);
do_transform(Form, _) ->
	Form.

%% Literals
istype_to_gaurd(Line, Var, {Type, _, Value}, _) ->
	{op, Line, '=:=', {var, Line, Var}, {Type, Line, Value}};

%% Simple types
istype_to_gaurd(Line, Var, {type, _, atom, _}, _) ->
	{call, Line, {atom, Line, is_atom}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, binary, _}, _) ->
	{call, Line, {atom, Line, is_binary}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, bitstring, _}, _) ->
	{call, Line, {atom, Line, is_bitstring}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, boolean, _}, _) ->
	{call, Line, {atom, Line, is_boolean}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, float, _}, _) ->
	{call, Line, {atom, Line, is_float}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, function, _}, _) ->
	{call, Line, {atom, Line, is_function}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, integer, _}, _) ->
	{call, Line, {atom, Line, is_integer}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, list, _}, _) ->
	{call, Line, {atom, Line, is_list}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, map, _}, _) ->
	{call, Line, {atom, Line, is_map}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, number, _}, _) ->
	{call, Line, {atom, Line, is_number}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, pid, _}, _) ->
	{call, Line, {atom, Line, is_pid}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, port, _}, _) ->
	{call, Line, {atom, Line, is_port}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, record, [{atom, _, RecordType}]}, _) ->
	{call, Line, {atom, Line, is_record}, [{var, Line, Var}, {atom, Line, RecordType}]};
istype_to_gaurd(Line, Var, {type, _, reference, _}, _) ->
	{call, Line, {atom, Line, is_reference}, [{var, Line, Var}]};
istype_to_gaurd(Line, Var, {type, _, tuple, _}, _) ->
	{call, Line, {atom, Line, is_tuple}, [{var, Line, Var}]};

%% Compound types
istype_to_gaurd(Line, Var, {type, _, range, [{LowType, _, Low}, {HighType, _, High}]}, _) ->
	{op, Line, 'andalso',
		{call, Line, {atom, Line, is_integer}, [{var, Line, Var}]},
		{op, Line, 'orelse',
			{op, Line, '>=', {var, Line, Var}, {LowType, Line, Low}},
			{op, Line, '=<', {var, Line, Var}, {HighType, Line, High}}}};

istype_to_gaurd(Line, Var, {type, _, union, [Type1, Type2]}, UserTypes) ->
	{op, Line, 'orelse',
		istype_to_gaurd(Line, Var, Type1, UserTypes),
		istype_to_gaurd(Line, Var, Type2, UserTypes)};
istype_to_gaurd(Line, Var, {type, _, union, [Type | Types]}, UserTypes) ->
	{op, Line, 'orelse',
		istype_to_gaurd(Line, Var, Type, UserTypes),
		istype_to_gaurd(Line, Var, {type, Line, union, Types}, UserTypes)};
	
istype_to_gaurd(Line, Var, {_, _, Type, _}, UserTypes) ->
	istype_to_gaurd(Line, Var, maps:get(Type, UserTypes), UserTypes).
