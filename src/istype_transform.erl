-module(istype_transform).
-export([parse_transform/2]).

%%====================================================================
%% parse_transform api
%%====================================================================
parse_transform(Forms, _Options) ->
    UserTypes = forms:reduce(fun get_types/2, #{types => #{}, records => #{}}, Forms),
    forms:map(fun(Form) -> do_transform(Form, UserTypes) end, Forms).

do_transform({call, Line, {atom, _, istype}, [Value, {call, _, {atom, _, Type}, TypeArgs}]}, UserTypes) ->
    %% is_integer(Value) orelse is_boolean(Value) ...
    %%
    %% OR
    %%
    %% begin
    %%     __IsType_1 = expression(),
    %%     is_integer(__IsType_1) orelse is_boolean(__IsType_1) ...
    %% end
    optimal_istype_to_guard(Line, setelement(2, Value, Line), {type, Line, Type, TypeArgs}, UserTypes);
do_transform({call, Line, {atom, _, totype}, [Value, {call, _, {atom, _, Type}, TypeArgs}]}, UserTypes) ->
    %% try
    %%     __IsType_1 = totype:convert(Value, TypeInfo),
    %%     asserttype(__IsType_1, type()),
    %%     __IsTYpe_1
    %% catch
    %%     error:{badmatch, false} ->
    %%         error({totype_conversion, type(), __IsType_1})
    %% end
    Converted = get_var(Line),
    {'try', Line, 
        [{match, Line, 
             Converted,
             {call, Line,
                 {remote, Line,
                     {atom, Line, totype},
                     {atom, Line, convert}},
                 [Value,
                  type_tree_node(Line, {type, Line, Type, TypeArgs}, UserTypes),
                  erl_parse:abstract(UserTypes, [{line, Line}])]}},
         do_transform({call, Line,
                          {atom, Line, asserttype},
                          [Converted, {call, Line, {atom, Line, Type}, TypeArgs}]},
                      UserTypes),
         Converted],
        [],
        [{clause, Line,
             [{tuple, Line,
                  [{atom, Line, error},
                   {tuple, Line,
                       [{atom, Line, badmatch},
                        {atom, Line, false}]},
                   {var, Line, '_'}]}],
             [],
             [{call, Line,
                  {atom, Line, error},
                  [{tuple, Line, [{atom, Line, totype_conversion},
                                  {atom, Line, Type},
                                  Value]}]}]}],
        []};        
do_transform({call, Line, {atom, _, asserttype}, Args}, UserTypes) ->
    %% true = istype(Value, type())
    {match, Line, 
        {atom, Line, true},
        do_transform({call, Line, {atom, Line, istype}, Args}, UserTypes)};
do_transform(Form, _) ->
    Form.

%%====================================================================
%% istype parse transform
%%====================================================================
%% @doc Optimize istype_to_guard by executing expressions prior to evaulating the
%%      results type. This optimization will break istype within guards so don't 
%%      process it if the expressions is a call to a BIF allowed in guards. 
%% @end
optimal_istype_to_guard(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, UserTypes) when length(Args) =:= 0 andalso
                                                                                             (BIF =:= node orelse
                                                                                              BIF =:= self) ->    
    istype_to_guard(Line, Value, Type, UserTypes);
optimal_istype_to_guard(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, UserTypes) when length(Args) =:= 1 andalso 
                                                                                             (BIF =:= abs orelse
                                                                                              BIF =:= bit_size orelse
                                                                                              BIF =:= byte_size orelse
                                                                                              BIF =:= ceil orelse
                                                                                              BIF =:= float orelse
                                                                                              BIF =:= floor orelse
                                                                                              BIF =:= hd  orelse
                                                                                              BIF =:= is_atom orelse
                                                                                              BIF =:= is_binary orelse
                                                                                              BIF =:= is_bitstring orelse
                                                                                              BIF =:= is_boolean orelse
                                                                                              BIF =:= is_float orelse
                                                                                              BIF =:= is_function orelse
                                                                                              BIF =:= is_integer orelse
                                                                                              BIF =:= is_list orelse
                                                                                              BIF =:= is_map orelse
                                                                                              BIF =:= is_number orelse
                                                                                              BIF =:= is_pid orelse
                                                                                              BIF =:= is_port orelse
                                                                                              BIF =:= is_reference orelse
                                                                                              BIF =:= is_tuple orelse
                                                                                              BIF =:= length orelse
                                                                                              BIF =:= map_size orelse
                                                                                              BIF =:= node orelse
                                                                                              BIF =:= round orelse
                                                                                              BIF =:= size orelse
                                                                                              BIF =:= tl orelse
                                                                                              BIF =:= trunc orelse
                                                                                              BIF =:= tuple_size) ->
    istype_to_guard(Line, Value, Type, UserTypes);
optimal_istype_to_guard(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, UserTypes) when length(Args) =:= 2 andalso
                                                                                             (BIF =:= binary_part orelse
                                                                                              BIF =:= element orelse
                                                                                              BIF =:= is_function orelse
                                                                                              BIF =:= is_record) ->
    istype_to_guard(Line, Value, Type, UserTypes);
optimal_istype_to_guard(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, UserTypes) when length(Args) =:= 3 andalso
                                                                                             (BIF =:= binary_part orelse
                                                                                              BIF =:= is_record) ->
    istype_to_guard(Line, Value, Type, UserTypes);
optimal_istype_to_guard(Line, Value, Type, UserTypes) when element(1, Value) =:= 'block' orelse
                                                           element(1, Value) =:= call    orelse
                                                           element(1, Value) =:= 'case'  orelse
                                                           element(1, Value) =:= 'try' ->
    Var = get_var(Line),
    Steps = [{match, Line, Var, Value},
             istype_to_guard(Line, Var, Type, UserTypes)],
    {block, Line, Steps};
optimal_istype_to_guard(Line, Var, Type, UserTypes) ->
    istype_to_guard(Line, Var, Type, UserTypes).

%% @doc Generate a guard friendly op that validates type. 
%% @end
%% @doc typespecs of tuple/3 are literal values.
%% @end
istype_to_guard(Line, Value, {_, _, _} = Literal, _) ->
    {op, Line, '=:=', Value, setelement(2, Literal, Line)};

%% @doc primitive types that we do not deeply inspect
%% @end
istype_to_guard(Line, Value, {type, _, atom, _}, _) ->
    istype_to_guard_bif(Line, Value, is_atom, []);
istype_to_guard(Line, Value, {type, _, binary, _}, _) ->
    istype_to_guard_bif(Line, Value, is_binary, []);
istype_to_guard(Line, Value, {type, _, bitstring, _}, _) ->
    istype_to_guard_bif(Line, Value, is_bitstring, []);
istype_to_guard(Line, Value, {type, _, boolean, _}, _) ->
    istype_to_guard_bif(Line, Value, is_boolean, []);
istype_to_guard(Line, Value, {type, _, float, _}, _) ->
    istype_to_guard_bif(Line, Value, is_float, []);
istype_to_guard(Line, Value, {type, _, function, _}, _) ->
    istype_to_guard_bif(Line, Value, is_function, []);
istype_to_guard(Line, Value, {type, _, integer, _}, _) ->
    istype_to_guard_bif(Line, Value, is_integer, []);
istype_to_guard(Line, Value, {type, _, list, _}, _) ->
    istype_to_guard_bif(Line, Value, is_list, []);
istype_to_guard(Line, Value, {type, _, map, _}, _) ->
    istype_to_guard_bif(Line, Value, is_map, []);
istype_to_guard(Line, Value, {type, _, number, _}, _) ->
    istype_to_guard_bif(Line, Value, is_number, []);
istype_to_guard(Line, Value, {type, _, pid, _}, _) ->
    istype_to_guard_bif(Line, Value, is_pid, []);
istype_to_guard(Line, Value, {type, _, port, _}, _) ->
    istype_to_guard_bif(Line, Value, is_port, []);
istype_to_guard(Line, Value, {type, _, reference, _}, _) ->
    istype_to_guard_bif(Line, Value, is_reference, []);

%% @doc primitive types that are easy to deeply inspect.
%% @end
%% @doc ranges define a set of valid integers
%% @end
istype_to_guard(Line, Value, {type, _, range, [{LowType, _, _} = Low, High]}, _) ->
    {op, Line, 'andalso',
        istype_to_guard(Line, Value, {type, Line, LowType, []}, []),
        {op, Line, 'andalso',
            {op, Line, '>=', Value, setelement(2, Low, Line)},
            {op, Line, '=<', Value, setelement(2, High, Line)}}};
%% @doc records are special tuples. Use the bif and then validate the fields.
%% @end
istype_to_guard(Line, Value, {type, _, record, [{atom, _, Record}]}, UserTypes) ->
    {RecordSize, _, RecordFieldSpec} = get_record_spec(Record, UserTypes),

    case RecordSize of
        1 ->
            istype_to_guard_bif(Line, Value, is_record, [{atom, Line, Record},
                                                              {integer, Line, RecordSize}]);
        _ -> 
            {op, Line, 'andalso',
                istype_to_guard_bif(Line, Value, is_record, [{atom, Line, Record},
                                                                  {integer, Line, RecordSize}]),
                record_guard(Line, Value, Record, RecordFieldSpec, UserTypes)}
    end;
%% @doc tuple spec is any tuple. don't do a deep inspection.
%% @end
istype_to_guard(Line, Value, {type, _, tuple, []}, UserTypes) ->
    istype_to_guard(Line, Value, {type, Line, tuple, any}, UserTypes);
istype_to_guard(Line, Value, {type, _, tuple, any}, _) ->
    {call, Line, {atom, Line, is_tuple}, [Value]};
%% @doc tuple spec is an empty tuple.
istype_to_guard(Line, Value, {type, _, tuple, none}, _) ->
    {op, Line, 'andalso',
                {call, Line, {atom, Line, 'is_tuple'}, [Value]},
                {op, Line, '=:=',
                    {call, Line, {atom, Line, tuple_size}, [Value]},
                    {integer, Line, 0}}};
%% @doc tuple is typed. Validate fields are correct.
%% @end  
istype_to_guard(Line, Value, {type, _, tuple, Types}, UserTypes) ->
    Length = length(Types),
    {op, Line, 'andalso',
        {call, Line, {atom, Line, 'is_tuple'}, [Value]},
        {op, Line, 'andalso',
            {op, Line, '=:=',
                {call, Line, {atom, Line, tuple_size}, [Value]},
                {integer, Line, Length}},
                tuple_guard(Line, 1, Value, Types, UserTypes)}};
%% @doc unions signify that one of many types/literals could be valid.
%%      validate that at least one of these types matches.
%% @end
istype_to_guard(Line, Value, {type, _, union, Types}, UserTypes) ->
    union_guard(Line, Value, Types, UserTypes);
%% @doc if no other clause has matched then we are not examining a primitive type.
%%      lookup the custom defined type and try again, repeating until we can validate
%%      against a primitive.
%% @end
istype_to_guard(Line, Value, {TypeClass, _, Type, _}, UserTypes) when TypeClass =:= type orelse
                                                                      TypeClass =:= user_type ->
    TypeSpec = get_type_spec(Type, UserTypes),
    istype_to_guard(Line, Value, TypeSpec, UserTypes).         

%% @doc call the bif that validates this type.
%% @end
istype_to_guard_bif(Line, Value, Guard, Args) ->
    {call, Line, {atom, Line, Guard}, [Value | Args]}.

%% @doc validate that all typed record fields are valid.
%% @end
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

%% @doc validate that all tuple fields are valid
%% @end
tuple_guard(Line, Index, Value0, [Type], UserTypes) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    istype_to_guard(Line, Value1, Type, UserTypes);
tuple_guard(Line, Index, Value0, [Type | Types], UserTypes) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    {op, Line, 'andalso',
        istype_to_guard(Line, Value1, Type, UserTypes),
        tuple_guard(Line, Index+1, Value0, Types, UserTypes)}.

%% @doc validate that at least one of the union types is valid
%% @end
union_guard(Line, Value, [Type1, Type2], UserTypes) ->
    {op, Line, 'orelse',
        istype_to_guard(Line, Value, Type1, UserTypes),
        istype_to_guard(Line, Value, Type2, UserTypes)};
union_guard(Line, Value, [Type | Types], UserTypes) ->
    {op, Line, 'orelse',
        istype_to_guard(Line, Value, Type, UserTypes),
        istype_to_guard(Line, Value, {type, Line, union, Types}, UserTypes)}.

%%====================================================================
%% totype parse transform
%%====================================================================
%% @doc Build a tree that can be traversed to reduce a type into it's
%%      primitive components. This will be used by the conversion function.
%% @end
%% @doc Wrap the typedate as either a type ot literal.
%% @end 
type_tree_node(Line, {_, _, _} = Literal, _) ->
    {tuple, Line,
        [{atom, Line, literal},
         setelement(2, Literal, Line)]};
type_tree_node(Line, TypeSpec, UserTypes) ->
    {tuple, Line,
        [{atom, Line, type},
         type_tree(Line, TypeSpec, UserTypes)]}.

%% @doc If it's a literal we must convert to that exact value.
%% @end
type_tree(_, {_, _, _} = Literal, _) ->
    Literal;
%% @doc simple primitives
%% @end
type_tree(Line, {_, _, Type, _}, _) when Type =:= atom orelse
                                         Type =:= binary orelse
                                         Type =:= bitstring orelse
                                         Type =:= boolean orelse
                                         Type =:= float orelse
                                         Type =:= integer orelse
                                         Type =:= list orelse
                                         Type =:= map orelse
                                         Type =:= number orelse
                                         Type =:= pid orelse
                                         Type =:= port orelse
                                         Type =:= reference ->
    {atom, Line, Type};
%% @doc range just needs to provide the bounds
%% @end
type_tree(Line, {_, _, range, Args}, _) ->
    {tuple, Line,
        [{atom, Line, range} |
         [setelement(2, X, Line) || X <- Args]]};
%% @doc records provide a mapping of typed fields to type data.
%% @end
type_tree(Line, {type, _, Type, _} = TypeSpec, UserTypes) when Type =:= record ->
    {_, _, _, [{atom, _, Record}]} = TypeSpec,
    {Arity, Fields, FieldTypes} = get_record_spec(Record, UserTypes),
    ArityMap = maps:from_list(lists:zip(Fields, lists:seq(2, length(Fields) + 1))),
    {tuple, Line, [
        {atom, Line, record},
        {atom, Line, Record},
        {record, Line, Record, []},
        {integer, Line, Arity},
        erl_parse:abstract(Fields, [{line, Line}]),
        erl_parse:abstract(ArityMap, [{line, Line}]),
        {map, Line, lists:map(fun({_, {_, _, {atom, _, Field}}, TypeSpec0}) ->
                                     {map_field_assoc, Line,
                                         {atom, Line, Field},
                                         type_tree_node(Line, TypeSpec0, UserTypes)};
                                 ({_, {_, _, {atom, _, Field}, _}, TypeSpec0}) ->
                                     {map_field_assoc, Line,
                                         {atom, Line, Field},
                                         type_tree_node(Line, TypeSpec0, UserTypes)}
                              end,
                              FieldTypes)}]};
%% @doc special tuple types
%% @end
type_tree(Line, {type, _, Type, Fields}, _) when Type =:= tuple andalso
                                                 (Fields =:= any orelse
                                                  Fields =:= none) ->
    {tuple, Line,
        [{atom, Line, Type},
         {atom, Line, Fields}]};
%% @doc if the type is a tuple we need to convert all fields.
%%      if the type is a union we need to convert to any 1 field.
%%      either way the logic to build out the types is the same.
%% @end
type_tree(Line, {type, _, Type, FieldTypes}, UserTypes) when Type =:= tuple orelse
                                                             Type =:= union ->
    {tuple, Line,
        [{atom, Line, Type},
         cons_map(fun(Line0, FieldType) ->
                      type_tree_node(Line0, FieldType, UserTypes)
                  end,
                  Line,
                  FieldTypes)]};
%% @doc the type is not a primitive, we need to examine it's components.
%% @end
type_tree(Line, {_, _, Type, _}, UserTypes) ->
    TypeSpec = get_type_spec(Type, UserTypes),
    type_tree(Line, TypeSpec, UserTypes).
        
%%====================================================================
%% utility functions
%%====================================================================
%% @doc generates a variable name that should be conflict free.
%% @end
get_var(Line) ->
    Var = case get('__var_counter__') of
              undefined -> 1;
              Counter -> Counter + 1
          end,
    put('__var_counter__', Var),
    {var, Line, list_to_atom("__IsType_" ++ integer_to_list(Var))}.

%% @doc Builds a collection of type and record definitions from the modules forms.
%% @end
%% @doc Slightly alter tuple typing to disambiguate between tuple() and {}
get_types({attribute, Line, type, {Type, {type, _, tuple, Args}, []}}, #{types := Acc1} = Acc0) ->
    FieldTypes = case Args of
                     any -> any;
                     [] -> none;
                     _ -> Args
                 end,
    Acc0#{types => Acc1#{Type => {type, Line, tuple, FieldTypes}}};
get_types({attribute, _, type, {Type, TypeSpec, []}}, #{types := Acc1} = Acc0) ->
    Acc0#{types => Acc1#{Type => TypeSpec}};
get_types({attribute, _, record, {Record, RecordSpec}}, #{records := Acc1} = Acc0) ->
    Acc0#{records => Acc1#{Record => {length(RecordSpec) + 1,
                                      lists:map(fun Fun({typed_record_field, RecordField, _}) ->
                                                        Fun(RecordField);
                                                    Fun({record_field, _, {atom, _, RecordField}}) ->
                                                        RecordField;
                                                    Fun({record_field, _, {atom, _, RecordField}, _}) ->
                                                        RecordField
                                                end,
                                                RecordSpec),
                                      lists:filter(fun(RecordField) when element(1, RecordField) =:= typed_record_field -> true;
                                                      (_) -> false
                                                   end,
                                                   RecordSpec)}}};
get_types(_, Acc) -> Acc.

%% @doc Retrieves the specified type from the found user types.
%%      An error is raised if the type does not exist.
%% @end
get_type_spec(Type, UserTypes) ->
    try
        #{types := #{Type := TypeSpec}} = UserTypes,
        TypeSpec
    catch
        error:{badmatch, _} ->
            io:format("Error when looking for typespec of ~p\n", [Type]),
            error({no_type, Type})
    end.

%% @doc Retrieves the specified type from the found user types.
%%      An error is raised if the type does not exist.
%% @end
get_record_spec(Record, UserTypes) ->
    try
        #{records := #{Record := RecordSpec}} = UserTypes,
        RecordSpec
    catch
        error:{badmatch, _} ->
            io:format("Error when looking for recordspec of ~p\n", [Record]),
            error({no_record, Record})
    end.

%% @doc Maps a list of values into an abstract list.
%% @end
cons_map(_, Line, []) ->
    {nil, Line};
cons_map(Fun, Line, [Hd | Tl]) ->
    {cons, Line,
        Fun(Line, Hd),
        cons_map(Fun, Line, Tl)}.
