-module(istype_transform).
-export([parse_transform/2]).

%%====================================================================
%% parse_transform api
%%====================================================================
parse_transform(Forms, _Options) ->
    %UserTypes = forms:reduce(fun get_types/2, #{types => #{}, records => #{}}, Forms),
    %forms:map(fun(Form) -> do_transform(Form, UserTypes) end, Forms).
    Records = forms:reduce(fun get_records/2, #{}, Forms),
    Types = forms:reduce(fun get_types/2,
                         #{iolist => {type, list, [{literal, atom, empty},
                                                   {type, union, [parse_type({type, 1, byte, []}),
                                                                  parse_type({type, 1, binary, []}),
                                                                  parse_type({type, 1, iolist, []})]},
                                                   {type, union, [parse_type({type, 1, binary, []}),
                                                                  parse_type({type, 1, nil, []})]}]}},
                         Forms),
    X = forms:map(fun(Form) ->
                  do_transform(Form, Types, Records)
              end,
              Forms),
    %io:format("~p\n", [Types]),
    X.

do_transform({call, Line, {atom, _, istype}, [Value, Type]}, Types, Records) ->
    %% is_integer(Value) orelse is_boolean(Value) ...
    %%
    %% OR
    %%
    %% begin
    %%     __IsType_1 = expression(),
    %%     is_integer(__IsType_1) orelse is_boolean(__IsType_1) ...
    %% end
    optimize_istype(Line, Value, parse_type(Type), Types, Records);
do_transform({call, Line, {atom, _, totype}, [Value, Type]}, Types, Records) ->
    %% try
    %%     __IsType_1 = totype:convert(Value, TypeInfo),
    %%     asserttype(__IsType_1, type()),
    %%     __IsTYpe_1
    %% catch
    %%     error:{badmatch, false} ->
    %%         error({istype_conversion, type(), __IsType_1})
    %% end
    Converted = get_var(Line),
    TypeConvertDataRecords = lists:map(fun({K, _}) ->
                                           TypeSpec = {type, record, {K, []}},
                                           {map_field_assoc, Line,
                                               {atom, Line, K},
                                               type_to_convert_data(Line, TypeSpec, Types, Records)}
                                       end,
                                       maps:to_list(Records)),
    {'try', Line,
        [{match, Line,
             Converted,
             {call, Line,
                 {remote, Line,
                     {atom, Line, istype_lib},
                     {atom, Line, convert}},
                 [Value,
                  type_to_convert_data(Line, parse_type(Type), Types, Records),
                  erl_parse:abstract(Types, [{line, Line}]),
                  {map, Line, TypeConvertDataRecords}]}},
         do_transform({call, Line,
                          {atom, Line, asserttype},
                          [Converted, Type]},
                      Types,
                      Records),
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
                  [{tuple, Line, [{atom, Line, istype_conversion},
                                  {atom, Line, Type},
                                  Value]}]}]}],
        []};
do_transform({call, Line, {atom, _, asserttype}, Args}, Types, Records) ->
    %% true = istype(Value, type())
    {match, Line,
        {atom, Line, true},
        do_transform({call, Line, {atom, Line, istype}, Args}, Types, Records)};
do_transform(Form, _, _) ->
    Form.

%%====================================================================
%% istype parse transform
%%====================================================================
%% @doc Optimize type_to_guard by executing expressions prior to evaulating the
%%      results type. This optimization will break istype within guards so don't
%%      process it if the expressions is a call to a BIF allowed in guards.
%% @endg
optimize_istype(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, Types, Records) when length(Args) =:= 0 andalso
                                                                                          (BIF =:= node orelse
                                                                                           BIF =:= self) ->
    type_to_guard(Line, Value, Type, Types, Records);
optimize_istype(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, Types, Records) when length(Args) =:= 1 andalso
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
    type_to_guard(Line, Value, Type, Types, Records);
optimize_istype(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, Types, Records) when length(Args) =:= 2 andalso
                                                                                          (BIF =:= binary_part orelse
                                                                                           BIF =:= element orelse
                                                                                           BIF =:= is_function orelse
                                                                                           BIF =:= is_record) ->
    type_to_guard(Line, Value, Type, Types, Records);
optimize_istype(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, Types, Records) when length(Args) =:= 3 andalso
                                                                                          (BIF =:= binary_part orelse
                                                                                           BIF =:= is_record) ->
    type_to_guard(Line, Value, Type, Types, Records);
optimize_istype(Line, Value, Type, Types, Records) when element(1, Value) =:= 'block' orelse
                                                        element(1, Value) =:= call    orelse
                                                        element(1, Value) =:= 'case'  orelse
                                                        element(1, Value) =:= 'try' ->
    Var = get_var(Line),
    Steps = [{match, Line, Var, Value},
             type_to_guard(Line, Var, Type, Types, Records)],
    {block, Line, Steps};
optimize_istype(Line, Value, Type, Types, Records) ->
    type_to_guard(Line, Value, Type, Types, Records).

%% @doc Generate a guard friendly expression that validates type.
%% @end
%% @doc typespecs of tuple/3 are literal values.
%% @end
type_to_guard(Line, Value, {literal, Type, LiteralValue}, _, _) ->
    {op, Line, '=:=', Value, {Type, Line, LiteralValue}};
%% @doc primitive types that we do not deeply inspect
%% @end
type_to_guard(Line, Value, {type, atom, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_atom, []);
type_to_guard(Line, Value, {type, binary, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_binary, []);
type_to_guard(Line, Value, {type, bitstring, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_bitstring, []);
type_to_guard(Line, Value, {type, boolean, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_boolean, []);
type_to_guard(Line, Value, {type, float, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_float, []);
type_to_guard(Line, Value, {type, function, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_function, []);
type_to_guard(Line, Value, {type, integer, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_integer, []);
type_to_guard(Line, Value, {type, list, []}, _, _) ->
    type_to_guard_bif(Line, Value, is_list, []);
type_to_guard(Line, Value, {type, map, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_map, []);
type_to_guard(Line, Value, {type, number, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_number, []);
type_to_guard(Line, Value, {type, pid, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_pid, []);
type_to_guard(Line, Value, {type, port, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_port, []);
type_to_guard(Line, Value, {type, reference, _}, _, _) ->
    type_to_guard_bif(Line, Value, is_reference, []);
%% @doc primitive types that are easy to deeply inspect.
%% @end
type_to_guard(Line, Value, {type, nil, []}, Types, Records) ->
    {op, Line, 'andalso',
        type_to_guard(Line, Value, {type, list, []}, Types, Records),
        {op, Line, '=:=',
            {call, Line, {atom, Line, length}, [Value]},
            {integer, Line, 0}}};
%% @doc ranges define a set of valid integers
%% @end
type_to_guard(Line, Value, {type, range, {undefined, High}}, Types, Records) ->
    {op, Line, 'andalso',
        type_to_guard(Line, Value, {type, integer, []}, Types, Records),
        {op, Line, '=<', Value, {integer, Line, High}}};
type_to_guard(Line, Value, {type, range, {Low, undefined}}, Types, Records) ->
    {op, Line, 'andalso',
        type_to_guard(Line, Value, {type, integer, []}, Types, Records),
        {op, Line, '>=', Value, {integer, Line, Low}}};
type_to_guard(Line, Value, {type, range, {Low, High}}, Types, Records) ->
    {op, Line, 'andalso',
        type_to_guard(Line, Value, {type, integer, []}, Types, Records),
        {op, Line, 'andalso',
            {op, Line, '>=', Value, {integer, Line, Low}},
            {op, Line, '=<', Value, {integer, Line, High}}}};
%% @doc records are special tuples. Use the bif and then validate the fields.
%% @end
type_to_guard(Line, Value, {type, record, {Record, Overrides}}, Types, Records) ->
    #{Record := RecordSpec} = Records,
    {Arity, RecordFields, RecordFieldTypes0} = RecordSpec,

    RecordFieldTypes1 = override_record_field_types(RecordFields, RecordFieldTypes0, Overrides),

    OrderedRecordFieldsTypes = lists:zip(lists:seq(2, Arity), RecordFieldTypes1),
    case Arity of
        1 ->
            type_to_guard_bif(Line, Value, is_record, [{atom, Line, Record},
                                                       {integer, Line, Arity}]);
        _ ->
            {op, Line, 'andalso',
                type_to_guard_bif(Line, Value, is_record, [{atom, Line, Record},
                                                           {integer, Line, Arity}]),
                tuple_guard(Line, Value, OrderedRecordFieldsTypes, Types, Records)}
    end;
%% @doc tuple spec is any tuple. don't do a deep inspection.
%% @end
type_to_guard(Line, Value, {type, tuple, []}, Types, Records) ->
    type_to_guard(Line, Value, {type, tuple, any}, Types, Records);
type_to_guard(Line, Value, {type, tuple, any}, _, _) ->
    {call, Line, {atom, Line, is_tuple}, [Value]};
%% @doc tuple spec is an empty tuple.
type_to_guard(Line, Value, {type, tuple, none}, _, _) ->
    {op, Line, 'andalso',
                {call, Line, {atom, Line, 'is_tuple'}, [Value]},
                {op, Line, '=:=',
                    {call, Line, {atom, Line, tuple_size}, [Value]},
                    {integer, Line, 0}}};
%% @doc tuple is typed. Validate fields are correct.
%% @end
type_to_guard(Line, Value, {type, tuple, TupleFieldTypes}, Types, Records) ->
    Size = length(TupleFieldTypes),
    OrderedTupleFieldsTypes = lists:zip(lists:seq(1, Size), TupleFieldTypes),
    {op, Line, 'andalso',
        {call, Line, {atom, Line, 'is_tuple'}, [Value]},
        {op, Line, 'andalso',
            {op, Line, '=:=',
                {call, Line, {atom, Line, tuple_size}, [Value]},
                {integer, Line, Size}},
                tuple_guard(Line, Value, OrderedTupleFieldsTypes, Types, Records)}};
%% @doc unions signify that one of many types/literals could be valid.
%%      validate that at least one of these types matches.
%% @end
type_to_guard(Line, Value, {type, union, UnionTypes}, Types, Records) ->
    union_guard(Line, Value, UnionTypes, Types, Records);
%% @doc examine complex primitives
type_to_guard(Line, Value, {type, list, [{type, any, _}]}, _, _) ->
    type_to_guard_bif(Line, Value, is_list, []);
type_to_guard(Line, Value, {type, list, ListSpec}, Types, Records) ->
    %io:format("List Spec ~p\n", [ListSpec]),
    {call, Line,
        {remote, Line,
            {atom, Line, istype_lib},
            {atom, Line, validate}},
        [Value,
         erl_parse:abstract({type, list, ListSpec}, [{line, Line}]),
         erl_parse:abstract(Types, [{line, Line}]),
         erl_parse:abstract(Records, [{line, Line}])]};
%% @doc if no other clause has matched then we are not examining a primitive type.
%%      lookup the custom defined type and try again, repeating until we can validate
%%      against a primitive.
%% @end
type_to_guard(Line, Value, {TypeClass, Type, _}, Types, Records) when TypeClass =:= type orelse
                                                                      TypeClass =:= user_type ->
    #{Type := TypeSpec} = Types,
    type_to_guard(Line, Value, TypeSpec, Types, Records).

%% @doc call the bif that validates this type.
%% @end
type_to_guard_bif(Line, Value, Guard, Args) ->
    {call, Line, {atom, Line, Guard}, [Value | Args]}.


%% @doc validate that all tuple fields are valid
%% @end
tuple_guard(Line, Value, FieldTypes, Types, Records) ->
    %% Any matches every value. Don't validate these indexes
    ValidatedFields = lists:filter(fun({_, {type, any, _}}) ->
                                          false;
                                      (_) ->
                                          true
                                   end,
                                   FieldTypes),
    gen_tuple_guard(Line, Value, ValidatedFields, Types, Records).

gen_tuple_guard(Line, Value0, [{Index, FieldType}], Types, Records) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    type_to_guard(Line, Value1, FieldType, Types, Records);
gen_tuple_guard(Line, Value0, [{Index, FieldType} | FieldTypes], Types, Records) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    {op, Line, 'andalso',
        type_to_guard(Line, Value1, FieldType, Types, Records),
        tuple_guard(Line, Value0, FieldTypes, Types, Records)}.

%% @doc validate that at least one of the union types is valid
%% @end
union_guard(Line, Value, UnionTypes, Types, Records) ->
    case is_any_union(UnionTypes, Types) of
        true ->
            {atom, Line, true};
        false ->
            gen_union_guard(Line, Value, UnionTypes, Types, Records)
    end.

%% @doc Inspect the union info to see if any field is an any type.
%%      If so then we can short circuit the entire check to true.
%% @end
is_any_union([], _) ->
    false;
is_any_union([{type, any, _} | _], _) ->
    true;
is_any_union([{type, union, InnerUnionTypes} | OuterUnionTypes], Types) ->
    case is_any_union(InnerUnionTypes, Types) of
        true ->
            true;
        false ->
            is_any_union(OuterUnionTypes, Types)
    end;
is_any_union([_ | UnionTypes], Types) ->
    is_any_union(UnionTypes, Types).

gen_union_guard(Line, Value, [UnionType1, UnionType2], Types, Records) ->
    {op, Line, 'orelse',
        type_to_guard(Line, Value, UnionType1, Types, Records),
        type_to_guard(Line, Value, UnionType2, Types, Records)};
gen_union_guard(Line, Value, [UnionType | UnionTypes], Types, Records) ->
    {op, Line, 'orelse',
        type_to_guard(Line, Value, UnionType, Types, Records),
        type_to_guard(Line, Value, {type, union, UnionTypes}, Types, Records)}.

%% @doc When specifying a record in a type spec or call it's possible to override
%%      the types of hte fields.
%% @end
override_record_field_types(RecordFields, RecordFieldTypes, Overrides) ->
    OverrideMap = maps:from_list(Overrides),
    lists:map(fun({Field, Type}) ->
                  case maps:get(Field, OverrideMap, undefined) of
                      undefined ->
                          Type;
                      Override ->
                          Override
                  end
              end,
              lists:zip(RecordFields, RecordFieldTypes)).

%%====================================================================
%% totype parse transform
%%====================================================================
%% @doc Build a tree that can be traversed to reduce a type into it's
%%      primitive components. This will be used by the conversion function.
%% @end
%% @doc Wrap the typedate as either a type of literal.
%% @end
type_to_convert_data(Line, {literal, _, _} = Literal, _, _) ->
    erl_parse:abstract(Literal, [{line, Line}]);
type_to_convert_data(Line, {_, Type, TypeArgs}, _, _) when Type =:= atom orelse
                                                           Type =:= binary orelse
                                                           Type =:= bitstring orelse
                                                           Type =:= boolean orelse
                                                           Type =:= float orelse
                                                           Type =:= integer orelse
                                                           Type =:= list orelse
                                                           Type =:= map orelse
                                                           Type =:= nil orelse
                                                           Type =:= number orelse
                                                           Type =:= pid orelse
                                                           Type =:= port orelse
                                                           Type =:= range orelse
                                                           Type =:= reference ->
    erl_parse:abstract({type, Type, TypeArgs}, [{line, Line}]);
type_to_convert_data(Line, {type, record, {Record, _Overrides}}, _, Records) ->
    #{Record := RecordSpec} = Records,
    {Arity, Fields, FieldTypes} = RecordSpec,
    ArityMap = maps:from_list(lists:zip(Fields, lists:seq(2, Arity))),
    FieldTuple = list_to_tuple([Record | FieldTypes]),
    {tuple, Line, [
        {atom, Line, type},
        {atom, Line, record},
        {tuple, Line, [
            {atom, Line, Record},
            {record, Line, Record, []},
            {integer, Line, Arity},
            erl_parse:abstract(ArityMap, [{line, Line}]),
            erl_parse:abstract(FieldTuple, [{line, Line}])]}]};
%% special ranges
%type_to_convert_data(Line, {type, byte, _}, Types, Records) ->
%    type_to_convert_data(Line, {type, range, {0, 255}}, Types, Records);
%type_to_convert_data(Line, {type, char, _}, Types, Records) ->
%    type_to_convert_data(Line, {type, range, {0, 16#10ffff}}, Types, Records);
%type_to_convert_data(Line, {type, neg_integer, _}, Types, Records) ->
%    type_to_convert_data(Line, {type, range, {undefined, -1}}, Types, Records);
%type_to_convert_data(Line, {type, non_neg_integer, _}, Types) ->
%    type_to_convert_data(Line, {type, range, {0, undefined}}, Types);
%type_to_convert_data(Line, {type, pos_integer, _}, Types) ->
%    type_to_convert_data(Line, {type, range, {1, undefined}}, Types);
%% @doc special tuple types
%% @end
type_to_convert_data(Line, {type, tuple, TupleFieldTypes}, _, _) when TupleFieldTypes =:= any orelse
                                                                      TupleFieldTypes =:= none ->
    erl_parse:abstract({type, tuple, TupleFieldTypes}, [{line, Line}]);
%% @doc if the type is a tuple we need to convert all fields.
%%      if the type is a union we need to convert to any 1 field.
%%      either way the logic to build out the types is the same.
%% @end
type_to_convert_data(Line, {type, Type, FieldTypes}, Types, Records) when Type =:= tuple orelse
                                                                          Type =:= union ->
    {tuple, Line,
        [{atom, Line, type},
         {atom, Line, Type},
         cons_map(fun(Line0, FieldType) ->
                      type_to_convert_data(Line0, FieldType, Types, Records)
                  end,
                  Line,
                  FieldTypes)]};
%% @doc the type is not a primitive, we need to examine it's components.
%% @end
type_to_convert_data(Line, {_, Type, _}, Types, Records) ->
    #{Type := TypeSpec} = Types,
    type_to_convert_data(Line, TypeSpec, Types, Records).

%%====================================================================
%% parsing functions
%%====================================================================
%% @doc Functions that are responsible for converting type specs and
%%      calls that represent type specs into the interlan type format.
%% @end
%% @doc Convert a raw literal value, a raw type, a call that represents
%%      a type, or a record that represents a type into a parsed_type.
%% @end
%% @doc Literal values. Should only ever be atom() or integer()
%% @end
parse_type({Type, _, Value}) ->
    {literal, Type, Value};
%% @doc Primitive types
%% @end
%% @doc atom - handled by the default handler.
%% @end
%% @doc integer - handled by the default handler.
%% @end
%% @doc float - handled by the default handler.
%% @end
%% @doc map - handled by the default handler.
%% @end
%% @doc pid - handled by the default handler.
%% @end
%% @doc port - handled by the default handler.
%% @end
parse_type({type, _, range, [{_, _, Low}, {_, _, High}]}) ->
    {type, range, {Low, High}};
%% @doc Record as seen in a type spec.
%% @end
parse_type({type, _, record, [{atom, _, Record} | FieldTypes]}) ->
    {type, record, {Record, lists:map(fun parse_type/1, FieldTypes)}};
%% @doc reference - handled by the default handler.
%% @end
%% @doc tuple that matches any tuple. tuple()
%% @end
parse_type({type, _, tuple, any}) ->
    {type, tuple, any};
%% @doc tuple that matches the empty tuple. {}
%% @end
parse_type({type, _, tuple, []}) ->
    {type, tuple, none};
%% @doc tuples with field types are handled by the default handler.
%% @end
%% @doc union - handled by the default handler.
%% @end
%% @doc 7.1: Built-in types, predefined aliases
%% @end
parse_type({type, _, term, _}) ->
    {type, any, []};
%% @doc binary - handled by the default handler.
%% @end
%% @doc bitstring - handled by the default handler.
%% @end
%% @doc boolean - handled by the default handler.
%% @end
parse_type({type, _, byte, _}) ->
    {type, range, {0, 255}};
parse_type({type, _, char, _}) ->
    {type, range, {0, 16#10ffff}};
%% @doc nil - handled by the default handler.
%% @end
%% @doc number - handled by the default handler.
%% @end
%% @doc list() - handled by the default handler.
%% @end
parse_type({type, Line, list, [ListType]}) ->
    parse_type({type, Line, listspec, [{atom, Line, empty}, ListType, {type, Line, nil, []}]});
parse_type({type, Line, maybe_improper_list, TypeArgs}) ->
    parse_type({type, Line, listspec, [{atom, Line, empty} | TypeArgs]});
parse_type({type, Line, nonempty_list, []}) ->
    parse_type({type, Line, listspec, [{atom, Line, nonempty}, {type, Line, any, []}, {type, Line, nil, []}]});
parse_type({type, Line, string, []}) ->
    parse_type({type, Line, listspec, [{atom, Line, empty}, {type, Line, char, []}, {type, Line, nil, []}]});
parse_type({type, Line, nonempty_string, []}) ->
    parse_type({type, Line, listspec, [{atom, Line, nonempty}, {type, Line, char, []}, {type, Line, nil, []}]});
parse_type({type, Line, iodata, _}) ->
    Type = {type, Line, union, [{type, Line, iolist, []},
                                {type, Line, binary, []}]},
    parse_type(Type);
%% @doc iolist - handled by the default handler.
%% @end
parse_type({type, _, function, _}) ->
    {type, 'fun', []};
parse_type({type, _, module, _}) ->
    {type, atom, []};
parse_type({type, Line, mfa, _}) ->
    Type = {type, Line, tuple, [{type, Line, module, []},
                                {type, Line, atom, []},
                                {type, Line, arity, []}]},
    parse_type(Type);
parse_type({type, _, arity, _}) ->
    {type, range, {0, 255}};
parse_type({type, Line, identifier, _}) ->
    Type = {type, Line, union, [{type, Line, pid, []},
                                {type, Line, port, []},
                                {type, Line, reference, []}]},
    parse_type(Type);
parse_type({type, _, node, _}) ->
    {type, atom, []};
parse_type({type, Line, timeout, _}) ->
    Type = {type, Line, union, [{atom, Line, infinity},
                                {type, Line, non_neg_integer, []}]},
    parse_type(Type);
%% @doc no_return - Not implemented yet, and not certain if possible...
%% @end
%% @doc 7.2: Additional built-in types
%% @end
parse_type({type, _, non_neg_integer, _}) ->
    {type, range, {0, undefined}};
parse_type({type, _, pos_integer, _}) ->
    {type, range, {1, undefined}};
parse_type({type, _, neg_integer, _}) ->
    {type, range, {undefined, -1}};
%% @doc Rarely used built-in types
%% @end
parse_type({type, Line, nonempty_maybe_improper_list, TypeArgs}) ->
    parse_type({type, Line, listspec, [{atom, Line, nonempty} | TypeArgs]});
parse_type({type, Line, nonempty_improper_list, TypeArgs}) ->
    parse_type({type, Line, listspec, [{atom, Line, nonempty} | TypeArgs]});
%% @doc Type components
%% @end
%% @doc field_type is a component of a record as a type.
%% @end
parse_type({type, _, field_type, [{atom, _, Field}, Type]}) ->
    {Field, parse_type(Type)};
%% @doc Internal type that represents the typing for a complex list.
%% @end
parse_type({type, _, listspec, ListTypes}) when length(ListTypes) =:= 3 ->
    {type, list, lists:map(fun parse_type/1, ListTypes)};
%% @doc Default type handler
%% @end
parse_type({Class, _, Type, TypeArgs}) when Class =:= type orelse
                                            Class =:= user_type ->
    {type, Type, lists:map(fun parse_type/1, TypeArgs)};
%% @doc When outside a typespec a call to a type is seen as a function call.
%%      Rework into a type and parse as usual.
%% @end
parse_type({call, Line, {atom, _, Type}, TypeArgs}) ->
    parse_type({type, Line, Type, TypeArgs});
%% @doc Records as types in a call.
%% @end
parse_type({record, _, Record, RecordArgs}) ->
    {type, record, {Record, lists:map(fun parse_type/1, RecordArgs)}}.

%% @doc Parse a record definition preserving necessary attributes.
%% @end
parse_record(RecordFields) ->
    Arity = length(RecordFields) + 1,
    Fields = lists:map(fun parse_record_field/1, RecordFields),
    FieldTypes = lists:map(fun parse_record_field_type/1, RecordFields),
    {Arity, Fields, FieldTypes}.

%% @doc Extract the name from a record field.
%% @end
parse_record_field({typed_record_field, RecordField, _}) ->
    parse_record_field(RecordField);
parse_record_field({record_field, _, {atom, _, RecordField}}) ->
    RecordField;
parse_record_field({record_field, _, {atom, _, RecordField}, _}) ->
    RecordField.

%% @doc Extract the type from a record field.
%% @end
parse_record_field_type({typed_record_field, _, Type}) ->
    parse_type(Type);
parse_record_field_type(_) ->
    parse_type({type, 1, any, []}).

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

%% @doc Function that generates a mapping of types to type specs.
%% @end
get_types({attribute, _, type, {Type, TypeSpec, _}}, Acc) ->
  Acc#{Type => parse_type(TypeSpec)};
get_types(_, Acc) ->
  Acc.

%% @doc Function that generates a mapping of records to record specs.
%% @end
get_records({attribute, _, record, {Record, RecordFields}}, Acc) ->
    Acc#{Record => parse_record(RecordFields)};
get_records(_, Acc) ->
    Acc.

%% @doc Maps a list of values into an abstract list.
%% @end
cons_map(_, Line, []) ->
    {nil, Line};
cons_map(Fun, Line, [Hd | Tl]) ->
    {cons, Line,
        Fun(Line, Hd),
        cons_map(Fun, Line, Tl)}.
