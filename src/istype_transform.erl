-module(istype_transform).
-export([parse_transform/2]).

%% @doc
%% @end
parse_transform(Forms, _Options) ->
    UserTypes = forms:reduce(fun get_types/2, #{types => #{}, records => #{}}, Forms),
    forms:map(fun(Form) -> do_transform(Form, UserTypes) end, Forms).

get_types({attribute, _, type, {Type, TypeSpec, []}}, #{types := Acc1} = Acc0) ->
    Acc0#{types => Acc1#{Type => TypeSpec}};
get_types({attribute, _, record, {Record, RecordSpec}}, #{records := Acc1} = Acc0) ->
    Acc0#{records => Acc1#{Record => {length(RecordSpec) + 1,
                                      lists:filter(fun(RecordField) when element(1, RecordField) =:= typed_record_field -> true;
                                                      (_) -> false
                                                   end,
                                                   RecordSpec)}}};
get_types(_, Acc) -> Acc.

do_transform({call, Line, {atom, _, istype}, [Value, {call, _, {atom, _, Type}, TypeArgs}]}, UserTypes) ->
    optimal_istype_to_guard(Line, setelement(2, Value, Line), {type, Line, Type, TypeArgs}, UserTypes);
do_transform({call, Line, {atom, _, asserttype}, Args}, UserTypes) ->
    {match, Line, 
        {atom, Line, true},
        do_transform({call, Line, {atom, Line, istype}, Args}, UserTypes)};
do_transform(Form, _) ->
    Form.

optimal_istype_to_guard(Line, {call, _, {atom, _, element} = Value, Args}, Type, UserTypes) when length(Args) =:= 2 ->
    istype_to_guard(Line, Value, Type, UserTypes);
optimal_istype_to_guard(Line, Value, Type, UserTypes) when element(1, Value) =:= call   orelse
                                                           element(1, Value) =:= 'case' orelse
                                                           element(1, Value) =:= 'block' ->
    Var = get_var(Line),
    Steps = [{match, Line, Var, Value},
             istype_to_guard(Line, Var, Type, UserTypes)],
    {block, Line, Steps};
optimal_istype_to_guard(Line, Var, Type, UserTypes) ->
    istype_to_guard(Line, Var, Type, UserTypes).

%% Literals
istype_to_guard(Line, Value, {_, _, _} = Literal, _) ->
    {op, Line, '=:=', Value, setelement(2, Literal, Line)};

%% Simple types
istype_to_guard(Line, Value, {type, _, atom, _}, _) ->
    istype_to_existing_guard(Line, Value, is_atom, []);
istype_to_guard(Line, Value, {type, _, binary, _}, _) ->
    istype_to_existing_guard(Line, Value, is_binary, []);
istype_to_guard(Line, Value, {type, _, bitstring, _}, _) ->
    istype_to_existing_guard(Line, Value, is_bitstring, []);
istype_to_guard(Line, Value, {type, _, boolean, _}, _) ->
    istype_to_existing_guard(Line, Value, is_boolean, []);
istype_to_guard(Line, Value, {type, _, float, _}, _) ->
    istype_to_existing_guard(Line, Value, is_float, []);
istype_to_guard(Line, Value, {type, _, function, _}, _) ->
    istype_to_existing_guard(Line, Value, is_function, []);
istype_to_guard(Line, Value, {type, _, integer, _}, _) ->
    istype_to_existing_guard(Line, Value, is_integer, []);
istype_to_guard(Line, Value, {type, _, list, _}, _) ->
    istype_to_existing_guard(Line, Value, is_list, []);
istype_to_guard(Line, Value, {type, _, map, _}, _) ->
    istype_to_existing_guard(Line, Value, is_map, []);
istype_to_guard(Line, Value, {type, _, number, _}, _) ->
    istype_to_existing_guard(Line, Value, is_number, []);
istype_to_guard(Line, Value, {type, _, pid, _}, _) ->
    istype_to_existing_guard(Line, Value, is_pid, []);
istype_to_guard(Line, Value, {type, _, port, _}, _) ->
    istype_to_existing_guard(Line, Value, is_port, []);
istype_to_guard(Line, Value, {type, _, reference, _}, _) ->
    istype_to_existing_guard(Line, Value, is_reference, []);

%% Compound types
istype_to_guard(Line, Value, {type, _, range, [{LowType, _, _} = Low, High]}, _) ->
    {op, Line, 'andalso',
        istype_to_guard(Line, Value, {type, Line, LowType, []}, []),
        {op, Line, 'andalso',
            {op, Line, '>=', Value, setelement(2, Low, Line)},
            {op, Line, '=<', Value, setelement(2, High, Line)}}};

istype_to_guard(Line, Value, {type, _, record, [{atom, _, RecordType}]}, UserTypes) ->
    #{records := #{RecordType := {RecordSize, RecordSpec}}} = UserTypes,

    case RecordSize of
        1 ->
            istype_to_existing_guard(Line, Value, is_record, [{atom, Line, RecordType},
                                                              {integer, Line, RecordSize}]);
        _ -> 
            {op, Line, 'andalso',
                istype_to_existing_guard(Line, Value, is_record, [{atom, Line, RecordType},
                                                                  {integer, Line, RecordSize}]),
                record_guard(Line, Value, RecordType, RecordSpec, UserTypes)}
    end;

istype_to_guard(Line, Value, {type, _, tuple, Types}, UserTypes) ->
    case length(Types) of
        0 ->
            {op, Line, 'andalso',
                {call, Line, {atom, Line, 'is_tuple'}, [Value]},
                {op, Line, '=:=',
                    {call, Line, {atom, Line, tuple_size}, [Value]},
                    {integer, Line, 0}}};
        Length ->
            {op, Line, 'andalso',
                {call, Line, {atom, Line, 'is_tuple'}, [Value]},
                {op, Line, 'andalso',
                    {op, Line, '=:=',
                        {call, Line, {atom, Line, tuple_size}, [Value]},
                        {integer, Line, Length}},
                    tuple_guard(Line, 1, Value, Types, UserTypes)}}
    end;

istype_to_guard(Line, Value, {type, _, union, Types}, UserTypes) ->
    union_guard(Line, Value, Types, UserTypes);

istype_to_guard(Line, Value, {TypeClass, _, Type, _}, UserTypes) when TypeClass =:= type orelse
                                                                      TypeClass =:= user_type ->
    #{types := #{Type := TypeSpec}} = UserTypes,
    istype_to_guard(Line, Value, TypeSpec, UserTypes).

istype_to_existing_guard(Line, Value, Guard, Args) ->
    {call, Line, {atom, Line, Guard}, [Value | Args]}.

get_var(Line) ->
    Var = case get('__var_counter__') of
              undefined -> 1;
              Counter -> Counter + 1
          end,
    put('__var_counter__', Var),
    {var, Line, list_to_atom("__IsType_" ++ integer_to_list(Var))}.

record_guard(Line, Value, RecordType, [RecordFieldSpec], UserTypes) ->
    {Field, Type} = case RecordFieldSpec of
                        {_, {_, _, F, _}, T} -> {F, T};
                        {_, {_, _, F}, T} -> {F, T}
                    end,
    istype_to_guard(Line, {record_field, Line, Value, RecordType, setelement(2, Field, Line)}, Type, UserTypes);
record_guard(Line, Value, RecordType, [RecordFieldSpec | RecordFieldSpecs], UserTypes) ->
    {Field, Type} = case RecordFieldSpec of
                        {_, {_, _, F, _}, T} -> {F, T};
                        {_, {_, _, F}, T} -> {F, T}
                    end,
    {op, Line, 'andalso',
        istype_to_guard(Line, {record_field, Line, Value, RecordType, setelement(2, Field, Line)}, Type, UserTypes),
        record_guard(Line, Value, RecordType, RecordFieldSpecs, UserTypes)}.

tuple_guard(Line, Index, Value0, [Type], UserTypes) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    istype_to_guard(Line, Value1, Type, UserTypes);

tuple_guard(Line, Index, Value0, [Type | Types], UserTypes) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    {op, Line, 'andalso',
        istype_to_guard(Line, Value1, Type, UserTypes),
        tuple_guard(Line, Index+1, Value0, Types, UserTypes)}.

union_guard(Line, Value, [Type1, Type2], UserTypes) ->
    {op, Line, 'orelse',
        istype_to_guard(Line, Value, Type1, UserTypes),
        istype_to_guard(Line, Value, Type2, UserTypes)};
union_guard(Line, Value, [Type | Types], UserTypes) ->
    {op, Line, 'orelse',
        istype_to_guard(Line, Value, Type, UserTypes),
        istype_to_guard(Line, Value, {type, Line, union, Types}, UserTypes)}.
