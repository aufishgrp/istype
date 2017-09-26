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
                         #{iolist => parse_type({type, 1, iolist, []})},
                         Forms),
    io:format("Parsed Types \n~p\n", [Types]),
    forms:map(fun(Form) ->
                  do_transform(Form, Types, Records)
              end,
              Forms).

do_transform({call, Line, {atom, _, istype}, [Value, Type]}, Types, Records) ->
    %% is_integer(Value) orelse is_boolean(Value) ...
    %%
    %% OR
    %%
    %% begin
    %%     __IsType_1 = expression(),
    %%     is_integer(__IsType_1) orelse is_boolean(__IsType_1) ...
    %% end
    optimize_istype(Line, Value, try_parse_type(Type), Types, Records);
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
                  type_to_convert_data(Line, try_parse_type(Type), Types, Records),
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
%% @doc Wrap the type data as either a type of literal.
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
         erl_parse:abstract(lists:map(fun(Line0, FieldType) ->
                                          type_to_convert_data(Line0, FieldType, Types, Records)
                                      end,
                                      FieldTypes),
                            [{line, Line}])]};
%% @doc the type is not a primitive, we need to examine it's components.
%% @end
type_to_convert_data(Line, {_, Type, _}, Types, Records) ->
    #{Type := TypeSpec} = Types,
    type_to_convert_data(Line, TypeSpec, Types, Records).

%%=============================================================================
%% parsing functions
%%=============================================================================
%% @doc Functions that are responsible for converting type specs and
%%      calls that represent type specs into the interlan type format.
%% @end

try_parse_type(Type) ->
    try
        io:format("Try parsing ~p\n", [Type]),
        parse_type(Type)
    catch
        Class:Error ->
            Stack = erlang:get_stacktrace(),
            io:format("Error when parsing ~p\n", [Type]),
            io:format("~p:~p\n", [Class, Error]),
            io:format("~p\n", [Stack]),
            error(badarg)
    end.

%%=========================================================
%% parse_type
%%=========================================================
%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
%%=====================================
%% any()
%%=====================================
%% @doc Expect :: {type, _, any, []}
%%              | {call, _, {atom, _, any}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% none()
%%=====================================
%% @doc Expect :: {type, _, none, []}
%%              | {call, _, {atom, _, none}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% pid()
%%=====================================
%% @doc Expect :: {type, _, pid, []}
%%              | {call, _, {atom, _, pid}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% port()
%%=====================================
%% @doc Expect :: {type, _, port, []}
%%              | {call, _, {atom, _, port}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% reference()
%%=====================================
%% @doc Expect :: {type, _, reference, []}
%%              | {call, _, {atom, _, reference}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% [] - nil()
%%=====================================
%% @doc Expect :: {type, _, nil, []}
%%              | {call, _, {atom, _, nil}, []}
%%              | TODO: Fill In Literal
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% Atom
%%=====================================
%% atom()
%%=================
%% @doc Expect :: {type, _, atom, []}
%%              | {call, _, {atom, _, atom}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=================
%% Erlang_Atom
%%=================
%% @doc Expect :: {atom, _, Erlang_Atom}
%%
%%      Erlang_Atoms need to be treated as literals.
%% @end
parse_type({atom, _, Atom}) ->
    {literal, atom, Atom};
%%=====================================
%% Bitstring
%%=====================================
%% @doc Expect :: <<>>
%%              | <<_:M>>
%%              | <<_:_*N>>
%%              | <<_:M, _:_*N>>
%%
%%      These patterns represent specific bitstrings formats.
%%      They need to be treated as literal values and should only
%%      be found within type specs.
%% @end
%%=====================================
parse_type({type, _, binary, [{integer, _, M}, {integer, _, N}]}) ->
    {type, bitstring, {M, N}};
%% float()
%%=====================================
%% @doc Expect :: {type, _, float, []}
%%              | {call, _, {atom, _, float}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% Fun
%%=====================================
%% @doc Expect :: {type, _, 'fun', []},
%%              | {call, _, {atom, _, 'fun'}, []},
%%              | {type, _, 'fun', [{type, _, any}, ReturnType]}
%%              | {type, _, 'fun', [{type, _, product, []}, ReturnType]}
%%              | {type, _, 'fun', [{type, _, product, ParameterTypes}, ReturnType]}
%%
%%      The forms for fun() will be handled by the default handler.
%% @end
parse_type({type, _, 'fun', [{type, _, any}, ReturnType]}) ->
    {type, 'fun', [any, parse_type(ReturnType)]};
parse_type({type, _, 'fun', [{type, _, product, ParameterTypes}, ReturnType]}) ->
    {type, 'fun', [lists:map(fun parse_type/1, ParameterTypes), parse_type(ReturnType)]};
%%=====================================
%% Integer
%%=====================================
%% @doc Expect :: {type, _, integer, []}
%%              | {call, _, {atom, _, integer}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% Erlang_Integer
%%=====================================
%% @doc Expect :: {integer, _, Erlang_Integer}
%%
%%      Erlang_Integers need to be treated as literals.
%% @end
parse_type({integer, _, Integer}) ->
    {literal, integer, Integer};
parse_type({op, _, '-', {integer, _, Integer}}) ->
    {literal, integer, -1 * Integer};
%%=====================================
%% Erlang_Integer..Erlang_Integer
%%=====================================
%% @doc Expect :: {type, _, range, [Erlang_Integer, Erlang_Integer]}
%%
%%      Ranges of Erlang_Integers.
%% @end
parse_type({type, _, range, [Low, High]}) ->
    {literal, _, LowBound} = parse_type(Low),
    {literal, _, HighBound} = parse_type(High),
    {type, range, {LowBound, HighBound}};
%%=====================================
%% List
%%=====================================
%% @doc Expect :: {type, _, list, [Type]}
%%              | {call, _, {atom, _, list}, [Type]}
%%              | {type, _, maybe_improper_list, [Type1, Type2]}
%%              | {call, _, {atom, _, maybe_improper_list}, [Type1, Type2]}
%%              | {type, _, nonempty_improper_list, [Type1, Type2]}
%%              | {call, _, {atom, _, nonempty_improper_list}, [Type1, Type2]}
%%              | {type, _, nonempty_list, [Type]}
%%              | {call, _, {atom, _, nonempty_list}, [Type]}
%%
%%      Lists can't be validated/converted in constant time. All Lists can be 
%%      validatd/converted with a common algorithm. Convert all List variants
%%      into an standardized internal format.
%%
%%      Calls are handled by the default call handler.
%% @end
parse_type({type, _, list, [Type]}) ->
    {type, list, [empty, parse_type(Type), parse_type({type, 1, nil, []})]};
parse_type({type, _, maybe_improper_list, Types}) ->
    {type, list, [empty | lists:map(fun parse_type/1, Types)]};
parse_type({type, _, nonempty_improper_list, Types}) ->
    {type, list, [nonempty | lists:map(fun parse_type/1, Types)]};
parse_type({type, _, nonempty_list, [Type]}) ->
    {type, list, [nonempty, parse_type(Type), parse_type({type, 1, nil, []})]};
%%=====================================
%% Map
%%=====================================
%% @doc Expect :: {type, _, map, any}           %% Any Map
%%              | {call, _, {atom, _, map}, []} %% Any Map
%%              | {type, _, map, []}            %% Empty Map
%%              | {type, _, map, MapFields}     %% Typed Map
%%
%%      MapFields :: list({type, _, map_field_assoc, Types})
%%                 | list({type, _, map_field_exact, Types})
%%      
%%      A call to map() is treated as any map.
%%      TODO: Add literals
%% @end
parse_type({type, _, map, any}) ->
    {type, map, any};
parse_type({call, _, {atom, _, map}, []}) ->
    {type, map, any};
parse_type({type, _, map, []}) ->
    {type, map, empty};
parse_type({type, _, map, MapFields}) ->
    {type, map, lists:map(fun parse_map_field/1, MapFields)};
%%=====================================
%% Tuple
%%=====================================
%% @doc Expect :: {type, _, tuple, any}           %% Any Tuple
%%              | {call, _, {atom, _, tuple}, []} %% Any Tuple
%%              | {type, _, tuple, []}            %% Empty Tuple
%%              | {type, _, tuple, FieldTypes}    %% Typed Tuple
%%
%%      TODO: Add literals
%% @end
parse_type({type, _, tuple, any}) ->
    {type, tuple, any};
parse_type({call, _, tuple, []}) ->
    {type, tuple, any};
parse_type({type, _, tuple, []}) ->
    {type, tuple, empty};
parse_type({type, _, tuple, FieldTypes}) ->
    {type, tuple, lists:map(fun parse_type/1, FieldTypes)};
%%=====================================
%% Union
%%=====================================
%% @doc Expect :: {type, _, union, Types} %% Any Tuple
%% @end
parse_type({type, _, union, Types}) ->
    {type, union, lists:map(fun parse_type/1, Types)};
%%=====================================
%% term()
%%=====================================
%% @doc Expect :: {type, _, term, []}
%%              | {call, _, {atom, _, term}, []}
%%      Alias for any().
%%
%%      Calls are handled by the default call handler.
%% @end
parse_type({type, Line, term, []}) ->
    parse_type({type, Line, any, []});
%%=====================================
%% binary()
%%=====================================
%% @doc Expect :: {type, _, binary, []}
%%              | {call, _, {atom, _, binary}, []}
%%
%%      Alias for <<_:_*8>>.
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% bitstring()
%%=====================================
%% @doc Expect :: {type, _, bitstring, []}
%%              | {call, _, {atom, _, bitstring}, []}
%%
%%      Alias for <<_:_*1>>.
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% boolean()
%%=====================================
%% @doc Expect :: {type, _, boolean, []}
%%              | {call, _, {atom, _, boolean}, []}
%%
%%      Alias for 'true' | 'false'.
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% byte()
%%=====================================
%% @doc Expect :: {type, _, byte, []}
%%              | {call, _, {atom, _, byte}, []}
%%
%%      Alias for 0..255.
%%
%%      Calls are handled by the default call handler.
%% @end
parse_type({type, Line, byte, []}) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                     {integer, Line, 255}]});
%%=====================================
%% char()
%%=====================================
%% @doc Expect :: {type, _, char, []}
%%              | {call, _, {atom, _, char}, []}
%%
%%      Alias for 0..16#110000.
%%
%%      Calls are handled by the default call handler.
%% @end
parse_type({type, Line, char, []}) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                     {integer, Line, 16#110000}]});
%%=====================================
%% nil()
%%=====================================
%% @doc Alias for [].
%%      All cases handled by [] above.
%% @end
%%=====================================
%% number()
%%=====================================
%% @doc Expect :: {type, _, number, []}
%%              | {call, _, {atom, _, number}, []}
%%
%%      Alias for integer() | float().
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% list()
%%=====================================
%% @doc Expect :: {type, _, list, []}
%%              | {call, _, {atom, _, list}, []}
%%
%%      Alias for list(any()).
%%
%%      Calls handled by List above.
%% @end
parse_type({type, Line, list, []}) ->
    parse_type({type, Line, list, [{type, Line, any, []}]});
%%=====================================
%% maybe_improper_list()
%%=====================================
%% @doc Expect :: {type, _, maybe_improper_list, []}
%%              | {call, _, {atom, _, maybe_improper_list}, []}
%%
%%      Alias for maybe_improper_list(any(), any()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, maybe_improper_list, []}) ->
    parse_type({type, Line, maybe_improper_list, [{type, Line, any, []},
                                                  {type, Line, any, []}]});
%%=====================================
%% nonempty_list()
%%=====================================
%% @doc Expect :: {type, _, nonempty_list, []}
%%              | {call, _, {atom, _, nonempty_list}, []}
%%
%%      Alias for nonempty_list(any()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, nonempty_list, []}) ->
    parse_type({type, Line, nonempty_list, [{type, Line, any, []}]});
%%=====================================
%% string()
%%=====================================
%% @doc Expect :: {type, _, string, []}
%%              | {call, _, {atom, _, string}, []}
%%
%%      Alias for list(char()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, string, []}) ->
    parse_type({type, Line, list, [{type, Line, char, []}]});
%%=====================================
%% nonempty_string()
%%=====================================
%% @doc Expect :: {type, _, nonempty_string, []}
%%              | {call, _, {atom, _, nonempty_string}, []}
%%
%%      Alias for [char, ...], aka, nonempty_list(char()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({Class, Line, nonempty_string, []}) ->
    parse_type({type, Line, nonempty_list, [{type, Line, char, []}]});
%%=====================================
%% iodata()
%%=====================================
%% @doc Expect :: {type, _, iodata, []}
%%              | {call, _, {atom, _, iodata}, []}
%%
%%      Alias for iolist() | binary().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, iodata, []}) ->
    parse_type({type, Line, union, [{type, Line, iolist, []},
                                    {type, Line, binary, []}]});
%%=====================================
%% iolist()
%%=====================================
%% @doc Expect :: {type, _, iolist, []}
%%              | {call, _, {atom, _, iolist}, []}
%%
%%      Internal :: {type, iolist, []}
%%
%%      Alias for maybe_improper_list(byte() | binary() | iolist(),
%%                                    binary() | [])
%%
%%      We have to expect the internal representation as well to
%%      prevent infinite recursion.
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, iolist, []}) ->
    Type1 = {type, Line, union, [{type, Line, byte, []},
                                 {type, Line, binary, []},
                                 {type, iolist, []}]},
    Type2 = {type, Line, union, [{type, Line, binary, []},
                                 {type, Line, nil, []}]},
    parse_type({type, Line, maybe_improper_list, [Type1, Type2]});
parse_type({type, iolist, []} = Iolist) ->
    Iolist;
%%=====================================
%% function()
%%=====================================
%% @doc Expect :: {type, _, function, []}
%%              | {call, _, {atom, _, function}, []}
%%
%%      Alias for fun().
%%      All cases can be handled by the default handler.
%% @end
%%=====================================
%% module()
%%=====================================
%% @doc Expect :: {type, _, module, []}
%%              | {call, _, {atom, _, module}, []}
%%
%%      Alias for atom().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, module, []}) ->
    parse_type({type, Line, atom, []});
%%=====================================
%% mfa()
%%=====================================
%% @doc Expect :: {type, _, mfa, []}
%%              | {call, _, {atom, _, mfa}, []}
%%
%%      Alias for {module(), atom(), arity()}.
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, mfa, []}) ->
    parse_type({type, Line, tuple, [{type, Line, module, []},
                                    {type, Line, atom, []},
                                    {type, Line, arity, []}]});
%%=====================================
%% arity()
%%=====================================
%% @doc Expect :: {type, _, arity, []}
%%              | {call, _, {atom, _, arity}, []}
%%
%%      Alias for 0..255.
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, arity, []}) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 255}]});
%%=====================================
%% identifier()
%%=====================================
%% @doc Expect :: {type, _, identifier, []}
%%              | {call, _, {atom, _, identifier}, []}
%%
%%      Alias for pid() | port() | reference().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, identifier, []}) ->
    parse_type({type, Line, union, [{type, Line, pid, []},
                                    {type, Line, port, []},
                                    {type, Line, reference, []}]});
%%=====================================
%% node()
%%=====================================
%% @doc Expect :: {type, _, node, []}
%%              | {call, _, {atom, _, node}, []}
%%
%%      Alias for atom().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, node, []}) ->
    parse_type({type, Line, atom, []});
%%=====================================
%% timeout()
%%=====================================
%% @doc Expect :: {type, _, timeout, []}
%%              | {call, _, {atom, _, timeout}, []}
%%
%%      Alias for 'infinity' | non_neg_integer().
%%
%%      Calls handled by the deafault call handler.
%% @end
parse_type({type, Line, timeout, []}) ->
    parse_type({type, Line, union, [{atom, Line, 'infinity'},
                                    {type, Line, non_neg_integer, []}]});
%%=====================================
%% no_return()
%%=====================================
%% @doc Expect :: {type, _, no_return, []}
%%              | {call, _, {atom, _, no_return}, []}
%%
%%      Alias for none().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, no_return, []}) ->
    parse_type({type, Line, none, []});
%%=====================================
%% non_neg_integer()
%%=====================================
%% @doc Expect :: {type, _, non_neg_integer, []}
%%              | {call, _, {atom, _, non_neg_integer}, []}
%%
%%      Alias for 0..
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, non_neg_integer, []}) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                     {atom, Line, undefined}]});
%%=====================================
%% pos_integer()
%%=====================================
%% @doc Expect :: {type, _, pos_integer, []}
%%              | {call, _, {atom, _, pos_integer}, []}
%%
%%      Alias for 1..
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, pos_integer, []}) ->
    parse_type({type, Line, range, [{integer, Line, 1},
                                     {atom, Line, undefined}]});
%%=====================================
%% neg_integer()
%%=====================================
%% @doc Expect :: {type, _, neg_integer, []}
%%              | {call, _, {atom, _, neg_integer}, []}
%%
%%      Alias for ..-1
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, neg_integer, []}) ->
    parse_type({type, Line, range, [{atom, Line, undefined},
                                     {integer, Line, -1}]});
%%=====================================
%% nonempty_maybe_improper_list()
%%=====================================
%% @doc Expect :: {type, _, nonempty_maybe_improper_list, []}
%%              | {call, _, {atom, _, nonempty_maybe_improper_list}, []}
%%
%%      Alias for nonempty_maybe_improper_list(any(), any()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, nonempty_maybe_improper_list, []}) ->
    parse_type({type, Line, nonempty_maybe_improper_list, [{type, Line, any, []},
                                                           {type, Line, any, []}]});
%%=====================================
%% nonempty_improper_list()
%%=====================================
%% @doc Expect :: {type, _, nonempty_improper_list, []}
%%              | {call, _, {atom, _, nonempty_improper_list}, []}
%%
%%      Handled in the List section above.
%% @end
%%=====================================
%% nonempty_maybe_improper_list(Type1, Type2)
%%=====================================
%% @doc Expect :: {type, _, nonempty_maybe_improper_list, [Type1, Type2]}
%%              | {call, _, {atom, _, nonempty_maybe_improper_list}, [Type1, Type2]}
%%
%%      Alias for nonempty_maybe_improper_list(any(), any()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, _, nonempty_maybe_improper_list, Types}) ->
    {type, list, [nonempty | lists:map(fun parse_type/1, Types)]};
%%=====================================
%% Record
%%=====================================
%% @doc Expect :: {type, _, record, [Record | FieldOverrides]}
%%              | {record, _, Record, FieldOverrides}
%% @end
parse_type({type, _, record, [{atom, _, Record} | RecordFields0]}) ->
    io:format("Parse Fields ~p\n", [RecordFields0]),
    RecordFields1 = lists:map(fun({type, _, field_type, [{atom, _, Field}, FieldType]}) ->
                                  {Field, parse_type(FieldType)}
                              end,
                              RecordFields0),
    {record, Record, RecordFields1};
parse_type({record, _, Record, RecordFields}) ->
    {record, Record, parse_record_fields(RecordFields)};

%%=====================================
%% Default call handler
%%=====================================
parse_type({call, Line, {atom, _, Type}, TypeArgs}) ->
    parse_type({type, Line, Type, TypeArgs});
%%=====================================
%% Default handler
%%=====================================
parse_type({Class, _, Type, TypeArgs}) when Class =:= type orelse
                                            Class =:= user_type ->
    {type, Type, lists:map(fun parse_type/1, TypeArgs)};

parse_type(Type) ->
    error(Type).

%%=========================================================
%% parse_record
%%=========================================================
parse_map_field({type, _, map_field_exact, Types}) ->
    list_to_tuple([require | lists:map(fun parse_type/1, Types)]);
parse_map_field({type, _, map_field_assoc, Types}) ->
    list_to_tuple([optional | lists:map(fun parse_type/1, Types)]).

%%=========================================================
%% parse_record
%%=========================================================
parse_record_fields(RecordFields) ->
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
  Acc#{Type => try_parse_type(TypeSpec)};
get_types(_, Acc) ->
  Acc.

%% @doc Function that generates a mapping of records to record specs.
%% @end
get_records({attribute, _, record, {Record, RecordFields}}, Acc) ->
    Acc#{Record => parse_record_fields(RecordFields)};
get_records(_, Acc) ->
    Acc.
