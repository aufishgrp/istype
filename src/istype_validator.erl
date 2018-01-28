-module(istype_validator).

-export([transform/3, transform/5]).

%%====================================================================
%% transform functions
%%====================================================================
%% transform
%%==========================================================
transform(Value, Type, Options) ->
    transform(Value, Type, #{}, #{}, Options).

-spec transform(Value :: istype:form(), Type :: istype:form(), istype:types(), istype:records(), istype:options()) -> istype:form().
%% @doc Generates a boolean statement that Value is of Type.
%% @end
%% @doc Start by determining if the type is an expressions that should be evaluated prior
%%      to checking its type. If so generate the block structure needed.
%% @end
transform({call, Line, {atom, _, BIF}, []} = Value, Type, Types, Records, Options) when BIF =:= node orelse
                                                                                        BIF =:= self ->
    transform(Line, Value, Type, Types, Records, Options);
transform({call, Line, {atom, _, BIF}, Args} = Value, Type, Types, Records, Options) when length(Args) =:= 1 andalso
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
    transform(Line, Value, Type, Types, Records, Options);
transform({call, Line, {atom, _, BIF}, Args} = Value, Type, Types, Records, Options) when length(Args) =:= 2 andalso
                                                                                          (BIF =:= binary_part orelse
                                                                                           BIF =:= element orelse
                                                                                           BIF =:= is_function orelse
                                                                                           BIF =:= is_record) ->
    transform(Line, Value, Type, Types, Records, Options);
transform({call, Line, {atom, _, BIF}, Args} = Value, Type, Types, Records, Options) when length(Args) =:= 3 andalso
                                                                                          (BIF =:= binary_part orelse
                                                                                           BIF =:= is_record) ->
    transform(Line, Value, Type, Types, Records, Options);
transform(Value, Type, Types, Records, Options) when element(1, Value) =:= 'block' orelse
                                                     element(1, Value) =:= call orelse
                                                     element(1, Value) =:= 'case' orelse
                                                     element(1, Value) =:= 'try' ->
    Line = element(2, Value),
    Var = istype_transform:get_var(Line),
    Steps = [{match, Line, Var, Value},
             transform(Line, Var, Type, Types, Records, Options)],
    {block, Line, Steps};
transform(Value, Type, Types, Records, Options) ->
    Line = element(2, Value),
    transform(Line, Value, Type, Types, Records, Options).

%%=========================================================
%% transform
%%=========================================================
%% @doc Generates a boolean expressions that transforms the
%%      given value is of the given type.
%% @end
%%=====================================
%% Literals
%%=====================================
%% @doc Expect :: {literal, Value}
%%
%%      Specific value comparison.
%% @end
transform(Line, Value, {literal, Literal}, _, _, _) ->
    {op, Line, '=:=', Value, Literal};
%%=====================================
%% any()
%%=====================================
%% @doc Expect :: {type, any, []}
%%
%%      any encompasses all values.
%% @end
transform(Line, _, {type, any, []}, _, _, _) ->
    {atom, Line, true};
%%=====================================
%% none()
%%=====================================
%% @doc Expect :: {type, any, []}
%%
%%      none is an empty set of values.
%% @end
transform(Line, _, {type, none, []}, _, _, _) ->
    {atom, Line, false};
%%=====================================
%% pid()
%%=====================================
%% @doc Expect :: {type, pid, []}
%% @end
transform(Line, Value, {type, pid, []}, _, _, _) ->
    {call, Line, {atom, Line, is_pid}, [Value]};
%%=====================================
%% port()
%%=====================================
%% @doc Expect :: {type, port, []}
%% @end
transform(Line, Value, {type, port, []}, _, _, _) ->
    {call, Line, {atom, Line, is_port}, [Value]};
%%=====================================
%% reference()
%%=====================================
%% @doc Expect :: {type, reference, []}
%% @end
transform(Line, Value, {type, reference, []}, _, _, _) ->
    {call, Line, {atom, Line, is_reference}, [Value]};
%%=====================================
%% []
%%=====================================
%% @doc Handled by the literal handler.
%% @end
%%=====================================
%% Atom
%%=====================================
%%=================
%% atom()
%%=================
%% @doc Expect :: {type, atom, []}
%% @end
transform(Line, Value, {type, atom, []}, _, _, _) ->
    {call, Line, {atom, Line, is_atom}, [Value]};
%%=================
%% Erlang_Atom
%%=================
%% @doc Handled by the literal handler.
%% @end
%%=====================================
%% Bitstring
%%=====================================
%% @doc Expect :: {type, bitstring, {M, N}}
%% @end
transform(Line, Value, {type, bitstring, {0, 0}}, _, _, _) ->
    {op, Line, '=:=', Value, {bin, Line, []}};

transform(Line, Value, {type, bitstring, {M, 0}}, _, _, _) ->
    {op, Line, 'andalso',
        {call, Line, {atom, Line, is_bitstring}, [Value]},
        {op, Line, '=:=',
            {call, Line, {atom, Line, bit_size}, [Value]},
            {integer, Line, M}}};

transform(Line, Value, {type, bitstring, {M, N}}, _, _, _) ->
    {op, Line, 'andalso',
        {call, Line, {atom, Line, is_bitstring}, [Value]},
        {op, Line, '=:=',
            {op, Line, 'rem',
                {call, Line, {atom, Line, bit_size}, [Value]},
                {integer, Line, N}},
            {integer, Line, M}}};
%%=====================================
%% float()
%%=====================================
%% @doc Expect :: {type, float, []}
%% @end
transform(Line, Value, {type, float, []}, _, _, _) ->
    {call, Line, {atom, Line, is_float}, [Value]};
%%=====================================
%% fun()
%%=====================================
%% @doc Expect :: {type, fun, []}
%%              | {type, fun, [ParameterTypes, ReturnType]}
%%
%%      For now we will only transform that the given Value is a
%%      fun().
%% @end
transform(Line, Value, {type, 'fun', []}, _, _, _) ->
    {call, Line, {atom, Line, is_function}, [Value]};
transform(Line, Value, {type, 'fun', _}, _, _, _) ->
    {call, Line, {atom, Line, is_function}, [Value]};
%%=====================================
%% Integer
%%=====================================
%% integer()
%%=================
%% @doc Expect :: {type, integer, []}
%% @end
transform(Line, Value, {type, integer, []}, _, _, _) ->
    {call, Line, {atom, Line, is_integer}, [Value]};
%%=================
%% Erlang_Integer
%%=================
%% @doc Handled by the literal handler.
%% @end
%%=================
%% Erlang_Integer..ErlangInteger
%%=================
%% @doc Expect :: {type, range, {LowerBound, UpperBound}}
%% @end
transform(Line, Value, {type, range, {{literal, {atom, _, undefined}}, {literal, UpperBound}}}, Types, Records, Options) ->
    {op, Line, 'andalso',
        transform(Line, Value, {type, integer, []}, Types, Records, Options),
        {op, Line, '=<', Value, UpperBound}};
transform(Line, Value, {type, range, {{literal, LowerBound}, {literal, {atom, _, undefined}}}}, Types, Records, Options) ->
    {op, Line, 'andalso',
        transform(Line, Value, {type, integer, []}, Types, Records, Options),
        {op, Line, '>=', Value, LowerBound}};
transform(Line, Value, {type, range, {{literal, LowerBound}, {literal, UpperBound}}}, Types, Records, Options) ->
   {op, Line, 'andalso',
        transform(Line, Value, {type, integer, []}, Types, Records, Options),
        {op, Line, 'andalso',
            {op, Line, '>=', Value, LowerBound},
            {op, Line, '=<', Value, UpperBound}}};
%%=====================================
%% List
%%=====================================
%% @doc Expect :: {type, list, [maybe_empty | nonempty, ValueType, TerminatorType]}
%%
%%      The typings [maybe_empty, {type, any, []}, {literal, {nil, _}}}] and
%%                  [nonempty, {type, any, []}, {literal, {nil, _}}}]
%%      can be evaluated in a guard safe manner.
%%
%%      All other lists need to be evaluated by istype_lib:transform.
%% @end
transform(Line, Value, {type, list, {maybe_empty, {type, any, []}, {type, any, []}}}, _, _, _) ->
    {call, Line, {atom, Line, is_list}, [Value]};
transform(Line, Value, {type, list, {maybe_empty, {type, any, []}, {literal, {nil, _}}}}, _, _, _) ->
    {call, Line, {atom, Line, is_list}, [Value]};
transform(Line, Value, {type, list, {nonempty, {type, any, []}, {type, any, []}}}, _, _, _) ->
    {op, Line, 'andalso',
        {call, Line, {atom, Line, is_list}, [Value]},
        {op, Line, '<',
            {integer, Line, 0},
            {call, Line, {atom, Line, length}, [Value]}}};
transform(Line, Value, {type, list, {nonempty, {type, any, []}, {literal, {nil, _}}}}, _, _, _) ->
    {op, Line, 'andalso',
        {call, Line, {atom, Line, is_list}, [Value]},
        {op, Line, '<',
            {integer, Line, 0},
            {call, Line, {atom, Line, length}, [Value]}}};
transform(Line, Value, {type, list, _} = Type, Types, Records, Options) ->
    transform_deep(Line, Value, Type, Types, Records, Options);
%%=====================================
%% Map
%%=====================================
%% @doc Expect :: {literal, map, Map}
%%              | {type, map, any}
%%              | {type, map, empty}
%%              | {type, map, MapFieldSpec}
%%
%%      Map specs for any and empty can be evaluated in a guard safe manner.
%%      All other maps need to be evaluated by istype_lib:transform.
%% @end
transform(Line, Value, {type, map, any}, _, _, _) ->
    {call, Line, {atom, Line, is_map}, [Value]};
transform(Line, Value, {type, map, empty}, _, _, _) ->
    {op, Line, '=:=',
        Value,
        erl_parse:abstract(#{}, [{line, Line}])};
transform(Line, Value, {type, map, _} = Type, Types, Records, Options) ->
    transform_deep(Line, Value, Type, Types, Records, Options);
%%======================================
%% Tuple
%%======================================
%% @doc Expect :: {type, tuple, any}
%%              | {type, tuple, empty}
%%              | {type, tuple, TupleFieldSpec}
%% @end
transform(Line, Value, {type, tuple, any}, _, _, _) ->
    {call, Line, {atom, Line, is_tuple}, [Value]};
transform(Line, Value, {type, tuple, empty}, _, _, _) ->
    {op, Line, '=:=',
        Value,
        {tuple, Line, []}};
transform(Line, Value, {type, tuple, TupleFieldSpec}, Types, Records, Options) ->
    %% Only transform fields that aren't the any type.
    ValidatedFields = lists:filter(fun({_, Type}) ->
                                       not is_any(Type, Types)
                                   end,
                                   lists:zip(lists:seq(1, length(TupleFieldSpec)), TupleFieldSpec)),
    case length(ValidatedFields) of
        0 ->
            {op, Line, 'andalso',
                {call, Line, {atom, Line, is_tuple}, [Value]},
                {op, Line, '=:=',
                    {integer, Line, length(TupleFieldSpec)},
                    {call, Line, {atom, Line, size}, [Value]}}};
        _ ->
            {op, Line, 'andalso',
                {call, Line, {atom, Line, is_tuple}, [Value]},
                {op, Line, 'andalso',
                    {op, Line, '=:=',
                        {integer, Line, length(TupleFieldSpec)},
                        {call, Line, {atom, Line, size}, [Value]}},
                    transform_tuple(Line, Value, ValidatedFields, Types, Records, Options)}}
    end;

%%======================================
%% Union
%%======================================
%% @doc Expect :: {type, union, Types}
%% @end
transform(Line, Value, {type, union, UnionTypes}, Types, Records, Options) ->
    case is_any(Types, Types) of
        true ->
            {atom, Line, true};
        false ->
            transform_union(Line, Value, UnionTypes, Types, Records, Options)
    end;
%%======================================
%% term()
%%======================================
%% @doc Handled by any()
%% @end
%%======================================
%% binary()
%%======================================
%% @doc Expect :: {type, binary, []}
%% @end
transform(Line, Value, {type, binary, []}, _, _, _) ->
    {call, Line, {atom, Line, is_binary}, [Value]};
%%======================================
%% bitstring()
%%======================================
%% @doc Expect :: {type, bitstring, []}
%% @end
transform(Line, Value, {type, bitstring, []}, _, _, _) ->
    {call, Line, {atom, Line, is_bitstring}, [Value]};
%%======================================
%% boolean()
%%======================================
%% @doc Expect :: {type, boolean, []}
%% @end
transform(Line, Value, {type, boolean, []}, _, _, _) ->
    {call, Line, {atom, Line, is_boolean}, [Value]};
%%======================================
%% byte()
%%======================================
%% @doc Handled by Erlang_Integer..Erlang_Integer
%% @end
%%======================================
%% char()
%%======================================
%% @doc Handled by Erlang_Integer..Erlang_Integer
%% @end
%%======================================
%% nil()
%%======================================
%% @doc Handled by []
%% @end
%%======================================
%% number()
%%======================================
%% @doc Expect :: {type, number, []}
%% @end
transform(Line, Value, {type, number, []}, _, _, _) ->
    {call, Line, {atom, Line, is_number}, [Value]};
%%======================================
%% list()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% maybe_improper_list()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% nonempty_list()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% string()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% nonempty_string()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% iodata()
%%======================================
%% @doc Handled by Union
%% @end
%%======================================
%% iolist()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% function()
%%======================================
%% @doc Handled by fun()
%% @end
%%======================================
%% module()
%%======================================
%% @doc Handled by atom()
%% @end
%%======================================
%% mfa()
%%======================================
%% @doc Handled by tuple()
%% @end
%%======================================
%% arity()
%%======================================
%% @doc Handled by Erlang_Integer..Erlang_Integer
%% @end
%%======================================
%% identifier()
%%======================================
%% @doc Handled by Union
%% @end
%%======================================
%% timeout()
%%======================================
%% @doc Handled by Union
%% @end
%%======================================
%% no_return()
%%======================================
%% @doc Handled by none()
%% @end
%%======================================
%% non_neg_integer()
%%======================================
%% @doc Handled by Erlang_Integer..Erlang_Integer
%% @end
%%======================================
%% pos_integer()
%%======================================
%% @doc Handled by Erlang_Integer..Erlang_Integer
%% @end
%%======================================
%% neg_integer()
%%======================================
%% @doc Handled by Erlang_Integer..Erlang_Integer
%% @end
%%======================================
%% nonempty_maybe_improper_list()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% nonempty_improper_list()
%%======================================
%% @doc Handled by List
%% @end
%%======================================
%% nonempty_maybe_improper_list(Type1, Type2)
%%======================================
%% @doc Handled by List
%% @end
%%=====================================
%% Record
%%=====================================
%% @doc Expect :: {type, record, RecordInfo}
%% @end
transform(Line, Value, {type, record, {Record, Overrides}}, Types, Records, Options) ->
     RecordSpec = try
                      #{Record := X} = Records,
                      X
                  catch
                      _:_ ->
                          error({unknown_record, Record})
                  end,
    
    {Arity, Fields, FieldTypes0, _} = RecordSpec,
    FieldTypes1 = lists:foldl(fun({Field, Type}, Acc) ->
                                  Acc#{Field => Type}
                              end,
                              FieldTypes0,
                              Overrides),

    IsRecord = {call, Line, {atom, Line, is_record}, [Value, {atom, Line, Record}, {integer, Line, Arity}]},
    case Arity of
        1 ->
            IsRecord;
        _ ->
            {op, Line, 'andalso',
                IsRecord,
                transform_record(Line, Value, Record, Fields, FieldTypes1, Types, Records, Options)}
    end;

%%======================================
%% Custom handler
%%======================================
%% @doc Custom types need the definition looked up from the parsed types.
%% @end
transform(Line, Value, {Class, Type, _}, Types, Records, Options) when Class =:= type orelse
                                                                       Class =:= user_type ->
    TypeSpec = try
                   #{Type := X} = Types,
                   X
               catch
                   _:_ ->
                       error({unknown_type, Type})
               end,
    transform(Line, Value, TypeSpec, Types, Records, Options).

%%==========================================================
%% transform_tuple
%%==========================================================
%% @doc Generate a guard for tuples.
%% @end
transform_tuple(Line, Value0, [{Index, FieldType}], Types, Records, Options) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    transform(Line, Value1, FieldType, Types, Records, Options);
transform_tuple(Line, Value0, [{Index, FieldType} | FieldTypes], Types, Records, Options) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    {op, Line, 'andalso',
        transform(Line, Value1, FieldType, Types, Records, Options),
        transform_tuple(Line, Value0, FieldTypes, Types, Records, Options)}.

%%==========================================================
%% transform_union
%%==========================================================
%% @doc Generate a guard for unions.
%% @end
transform_union(Line, Value, [UnionType], Types, Records, Options) ->
    transform(Line, Value, UnionType, Types, Records, Options);
transform_union(Line, Value, [UnionType | UnionTypes], Types, Records, Options) ->
    {op, Line, 'orelse',
        transform(Line, Value, UnionType, Types, Records, Options),
        transform_union(Line, Value, UnionTypes, Types, Records, Options)}.

%%==========================================================
%% transform_record
%%==========================================================
%% @doc Generate a guard for records.
%% @end
transform_record(Line, Value0, Record, [Field], FieldTypes, Types, Records, Options) ->
    #{Field := FieldType} = FieldTypes,
    Value1 = {record_field, Line, Value0, Record, {atom, Line, Field}},
    transform(Line, Value1, FieldType, Types, Records, Options);

transform_record(Line, Value0, Record, [Field | Fields], FieldTypes, Types, Records, Options) ->
    #{Field := FieldType} = FieldTypes,
    Value1 = {record_field, Line, Value0, Record, {atom, Line, Field}},
    {op, Line, 'andalso',
        transform(Line, Value1, FieldType, Types, Records, Options),
        transform_record(Line, Value0, Record, Fields, FieldTypes, Types, Records, Options)}.

%%==========================================================
%% transform_deep
%%==========================================================
%% @doc Handler for maps and lists that need deep inspection.
%% @end
transform_deep(Line, Value, Type, Types, Records, Options) ->
    {call, Line,
        {remote, Line,
            {atom, Line, istype_lib},
            {atom, Line, transform}},
        [Value,
         erl_parse:abstract(Type, [{line, Line}]),
         erl_parse:abstract(Types, [{line, Line}]),
         erl_parse:abstract(Records, [{line, Line}]),
         erl_parse:abstract(Options, [{line, Line}])]}.

%%==========================================================
%% is_any
%%==========================================================
%% @doc Function that determines if a type resolves to any().
%% @end
is_any({type, any, _}, _) ->
    true;
is_any({type, union, UnionTypes}, Types) ->
    is_any(UnionTypes, Types);
is_any([{type, any, _} | _], _) ->
    true;
is_any([{type, union, InnerUnionTypes} | OuterUnionTypes], Types) ->
    is_any(InnerUnionTypes, Types) orelse is_any(OuterUnionTypes, Types);
is_any([_ | UnionTypes], Types) ->
    is_any(UnionTypes, Types);
is_any(_, _) ->
    false.
