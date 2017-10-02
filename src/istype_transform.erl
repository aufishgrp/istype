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
    X = optimize_istype(Line, Value, try_parse_type(Type), Types, Records),
    try
        forms:from_abstract(X)
    catch
        _:_ ->
            io:format("Bad form ~p\nGenerated from ~p\n", [X, {call, Line, Value, Type}]),
            halt()
    end,
    X;
do_transform({call, Line, {atom, _, totype}, [Value, Type0]}, Types, Records) ->
    %% try
    %%     __IsType_1 = istype_lib:totype(Value, TypeInfo),
    %%     asserttype(__IsType_1, type()),
    %%     __IsTYpe_1
    %% catch
    %%     error:{badmatch, false} ->
    %%         error({istype_conversion, type(), __IsType_1})
    %% end
    {_, Type, _} = Type1 = try_parse_type(Type0),
    Converted = get_var(Line),
    {'try', Line,
        [{match, Line,
             Converted,
             {call, Line,
                 {remote, Line,
                     {atom, Line, istype_lib},
                     {atom, Line, totype}},
                 [Value,
                  erl_parse:abstract(Type1, [{line, Line}]),
                  erl_parse:abstract(Types, [{line, Line}]),
                  erl_parse:abstract(Records, [{line, Line}])]}},
         do_transform({call, Line,
                          {atom, Line, asserttype},
                          [Converted, Type0]},
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
%% istype functions
%%====================================================================
%% optimize_istype
%%==========================================================
%% @doc Optimize istype by executing expressions prior to evaulating the
%%      results type. This optimization will break istype within guards so don't
%%      process it if the expressions is a call to a BIF allowed in guards.
%% @endg
optimize_istype(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, Types, Records) when length(Args) =:= 0 andalso
                                                                                          (BIF =:= node orelse
                                                                                           BIF =:= self) ->
    istype(Line, Value, Type, Types, Records);
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
    istype(Line, Value, Type, Types, Records);
optimize_istype(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, Types, Records) when length(Args) =:= 2 andalso
                                                                                          (BIF =:= binary_part orelse
                                                                                           BIF =:= element orelse
                                                                                           BIF =:= is_function orelse
                                                                                           BIF =:= is_record) ->
    istype(Line, Value, Type, Types, Records);
optimize_istype(Line, {call, _, {atom, _, BIF} = Value, Args}, Type, Types, Records) when length(Args) =:= 3 andalso
                                                                                          (BIF =:= binary_part orelse
                                                                                           BIF =:= is_record) ->
    istype(Line, Value, Type, Types, Records);
optimize_istype(Line, Value, Type, Types, Records) when element(1, Value) =:= 'block' orelse
                                                        element(1, Value) =:= call    orelse
                                                        element(1, Value) =:= 'case'  orelse
                                                        element(1, Value) =:= 'try' ->
    Var = get_var(Line),
    Steps = [{match, Line, Var, Value},
             istype(Line, Var, Type, Types, Records)],
    {block, Line, Steps};
optimize_istype(Line, Value, Type, Types, Records) ->
    istype(Line, Value, Type, Types, Records).

%%=========================================================
%% istype
%%=========================================================
%% @doc Generates a boolean expressions that validates the
%%      given value is of the given type.
%% @end
%%=====================================
%% any()
%%=====================================
%% @doc Expect :: {type, any, []}
%%
%%      any encompasses all values.
%% @end
istype(Line, _, {type, any, []}, _, _) ->
    {atom, Line, true};
%%=====================================
%% none()
%%=====================================
%% @doc Expect :: {type, any, []}
%%
%%      none is an empty set of values.
%% @end
istype(Line, _, {type, none, []}, _, _) ->
    {atom, Line, false};
%%=====================================
%% pid()
%%=====================================
%% @doc Expect :: {type, pid, []}
%% @end
istype(Line, Value, {type, pid, []}, _, _) ->
    {call, Line, {atom, Line, is_pid}, [Value]};
%%=====================================
%% port()
%%=====================================
%% @doc Expect :: {type, port, []}
%% @end
istype(Line, Value, {type, port, []}, _, _) ->
    {call, Line, {atom, Line, is_port}, [Value]};
%%=====================================
%% reference()
%%=====================================
%% @doc Expect :: {type, reference, []}
%% @end
istype(Line, Value, {type, reference, []}, _, _) ->
    {call, Line, {atom, Line, is_reference}, [Value]};
%%=====================================
%% []
%%=====================================
%% @doc Expect :: {literal, nil, []}
%%              | {type, nil, []}
%%
%%      Nil is a special type for the value [].
%% @end
istype(Line, Value, {Class, nil, []}, _, _) when Class =:= literal orelse 
                                                 Class =:= type ->
    {op, Line, '=:=',
        Value,
        {nil, Line}};
%%=====================================
%% Atom
%%=====================================
%%=================
%% atom()
%%=================
%% @doc Expect :: {type, atom, []}
%% @end
istype(Line, Value, {type, atom, []}, _, _) ->
    {call, Line, {atom, Line, is_atom}, [Value]};
%%=================
%% Erlang_Atom
%%=================
%% @doc Handled by the literal handler.
%% @end
%%=====================================
%% Bitstring
%%=====================================
%% @doc Handled by the literal handler.
%% @end
%%=====================================
%% float()
%%=====================================
%% @doc Expect :: {type, float, []}
%% @end
istype(Line, Value, {type, float, []}, _, _) ->
    {call, Line, {atom, Line, is_float}, [Value]};
%%=====================================
%% fun()
%%=====================================
%% @doc Expect :: {type, fun, []}
%%              | {type, fun, [ParameterTypes, ReturnType]}
%%
%%      For now we will only validate that the given Value is a
%%      fun().
%% @end
istype(Line, Value, {type, 'fun', []}, _, _) ->
    {call, Line, {atom, Line, is_fun}, [Value]};
%%=====================================
%% Integer
%%=====================================
%% integer()
%%=================
%% @doc Expect :: {type, integer, []}
%% @end
istype(Line, Value, {type, integer, []}, _, _) ->
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
istype(Line, Value, {type, range, {undefined, UpperBound}}, Types, Records) ->
    {op, Line, 'andalso',
        istype(Line, Value, {type, integer, []}, Types, Records),
        {op, Line, '=<', Value, {integer, Line, UpperBound}}};
istype(Line, Value, {type, range, {LowerBound, undefined}}, Types, Records) ->
    {op, Line, 'andalso',
        istype(Line, Value, {type, integer, []}, Types, Records),
        {op, Line, '>=', Value, {integer, Line, LowerBound}}};
istype(Line, Value, {type, range, {LowerBound, UpperBound}}, Types, Records) ->
   {op, Line, 'andalso',
        istype(Line, Value, {type, integer, []}, Types, Records),
        {op, Line, 'andalso',
            {op, Line, '>=', Value, {integer, Line, LowerBound}},
            {op, Line, '=<', Value, {integer, Line, UpperBound}}}};
%%=====================================
%% List
%%=====================================
%% @doc Expect :: {type, list, [maybe_empty | nonempty, ValueType, TerminatorType]}
%%
%%      The typings [maybe_empty, {type, any, []}, {type, nil, []}] and
%%                  [nonempty, {type, any, []}, {type, nil, []}]
%%      can be evaluated in a guard safe manner.
%%
%%      All other lists need to be evaluated by istype_lib:istype.
%% @end
istype(Line, Value, {type, list, [maybe_empty, {type, any, []}, {type, nil, []}]}, _, _) ->
    {call, Line, {atom, Line, is_list}, [Value]};
istype(Line, Value, {type, list, [nonempty, {type, any, []}, {type, nil, []}]}, _, _) ->
    {op, Line, 'andalso', 
        {call, Line, {atom, Line, is_list}, [Value]},
        {op, Line, '<',
            {integer, Line, 0},
            {call, Line, {atom, Line, length}, [Value]}}};
istype(Line, Value, {type, list, ListSpec}, Types, Records) ->
    istype(Line, Value, {deep_type, list, ListSpec}, Types, Records);
%%=====================================
%% Map
%%=====================================
%% @doc Expect :: {literal, map, Map}
%%              | {type, map, any}
%%              | {type, map, empty}
%%              | {type, map, MapFieldSpec}
%%
%%      Map specs for any and empty can be evaluated in a guard safe manner.
%%      All other maps need to be evaluated by istype_lib:istype.
%% @end
istype(Line, Value, {literal, map, Map}, _, _)->
    {op, Line, '=:=',
        Value,
        erl_parse:abstract(#{}, [{line, Line}])};
istype(Line, Value, {type, map, any}, _, _) ->
    {call, Line, {atom, Line, is_map}, [Value]};
istype(Line, Value, {type, map, empty}, _, _) ->
    {op, Line, '=:=',
        Value,
        erl_parse:abstract(#{}, [{line, Line}])};
istype(Line, Value, {type, map, MapSpec}, Types, Records) ->
    istype(Line, Value, {deep_type, map, MapSpec}, Types, Records);    
%%======================================
%% Tuple
%%======================================
%% @doc Expect :: {type, tuple, any}
%%              | {type, tuple, empty}
%%              | {type, tuple, TupleFieldSpec}
%% @end
istype(Line, Value, {type, tuple, any}, _, _) ->
    {call, Line, {atom, Line, is_tuple}, [Value]};
istype(Line, Value, {type, tuple, empty}, _, _) ->
    {op, Line, '=:=',
        Value,
        erl_parse:abstract({}, [{line, Line}])};
istype(Line, Value, {type, tuple, TupleFieldSpec}, Types, Records) ->
    %% Only validate fields that aren't the any type.
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
                    istype_tuple(Line, Value, ValidatedFields, Types, Records)}}                
    end;

%%======================================
%% Union
%%======================================
%% @doc Expect :: {type, union, Types}
%% @end
istype(Line, Value, {type, union, UnionTypes}, Types, Records) ->
    case is_any(Types, Types) of
        true ->
            {atom, Line, true};
        false ->
            istype_union(Line, Value, UnionTypes, Types, Records)
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
istype(Line, Value, {type, binary, []}, _, _) ->
    {call, Line, {atom, Line, is_binary}, [Value]};
%%======================================
%% bitstring()
%%======================================
%% @doc Expect :: {type, bitstring, []}
%% @end
istype(Line, Value, {type, bitstring, []}, _, _) ->
    {call, Line, {atom, Line, is_bitstring}, [Value]};
%%======================================
%% boolean()
%%======================================
%% @doc Expect :: {type, boolean, []}
%% @end
istype(Line, Value, {type, boolean, []}, _, _) ->
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
istype(Line, Value, {type, number, []}, _, _) ->
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
%% @doc Expect :: {record, Record, RecordInfo}
%% @end
istype(Line, Value, {record, Record, RecordInfo}, Types, Records) ->
    {Arity, Fields, FieldTypes} = RecordInfo,

    RecordFields = lists:filter(fun({_, FieldType}) ->
                                    not is_any(FieldType, Types)
                                end,
                                lists:zip(Fields, FieldTypes)),
    case length(RecordFields) of
        0 ->
            {call, Line, {atom, Line, is_record}, [
                    Value,
                    {atom, Line, Record},
                    {integer, Line, Arity}]};
        _ ->
            {op, Line, 'andalso',
                {call, Line, {atom, Line, is_record}, [
                    Value,
                    {atom, Line, Record},
                    {integer, Line, Arity}]},
                istype_record(Line, Value, Record, RecordFields, Types, Records)}
    end;
%%======================================
%% Deep type handler
%%======================================
%% @doc Handler for maps and lists that need deep inspection.
%% @end
istype(Line, Value, {deep_type, _, _} = TypeSpec, Types, Records) ->
    {call, Line,
        {remote, Line, 
            {atom, Line, istype_lib},
            {atom, Line, istype}},
        [Value,
         erl_parse:abstract(TypeSpec, [{line, Line}]),
         erl_parse:abstract(Types, [{line, Line}]),
         erl_parse:abstract(Records, [{line, Line}])]};
%%=====================================
%% Literals
%%=====================================
%% @doc Expect :: {literal, Type, Value}
%%
%%      Specific value comparison.
%% @end
istype(Line, Value, {literal, Type, Literal}, _, _) ->
    {op, Line, '=:=',
        Value,
        {Type, Line, Literal}};
%%======================================
%% Custom handler
%%======================================
%% @doc Custom types need the definition looked up from the parsed types.
%% @end
istype(Line, Value, {Class, Type, _}, Types, Records) when Class =:= type orelse
                                                           Class =:= user_type ->
    TypeSpec = try
                   #{Type := X} = Types,
                   X
               catch
                   _:_ ->
                       io:format("Type ~p not unknown.\n", [Type]),
                       halt()
               end,
    istype(Line, Value, TypeSpec, Types, Records);
istype(Line, Value, {record, Record, _}, Types, Records) ->
    RecordSpec = try
                     #{Record := X} = Records,
                     X
                 catch
                     _:_ ->
                         io:format("Record ~p not unknown.\n", [Record]),
                         halt()
                 end,
    istype(Line, Value, RecordSpec, Types, Records).

%%==========================================================
%% istype_tuple
%%==========================================================
%% @doc Generate a guard for tuples.
%% @end
istype_tuple(Line, Value0, [{Index, FieldType}], Types, Records) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    istype(Line, Value1, FieldType, Types, Records);
istype_tuple(Line, Value0, [{Index, FieldType} | FieldTypes], Types, Records) ->
    Value1 = {call, Line, {atom, Line, element}, [{integer, Line, Index}, Value0]},
    {op, Line, 'andalso',
        istype(Line, Value1, FieldType, Types, Records),
        istype_tuple(Line, Value0, FieldTypes, Types, Records)}.

%%==========================================================
%% istype_union
%%==========================================================
%% @doc Generate a guard for unions.
%% @end
istype_union(Line, Value, [UnionType], Types, Records) ->
    istype(Line, Value, UnionType, Types, Records);
istype_union(Line, Value, [UnionType | UnionTypes], Types, Records) ->
    {op, Line, 'orelse',
        istype(Line, Value, UnionType, Types, Records),
        istype_union(Line, Value, UnionTypes, Types, Records)}.

%%==========================================================
%% istype_record
%%==========================================================
%% @doc Generate a guard for records.
%% @end
istype_record(Line, Value0, Record, [RecordFieldSpec], Types, Records) ->
    {RecordField, RecordFieldType} = RecordFieldSpec,
    Value1 = {record_field, Line, Value0, Record, {atom, Line, RecordField}},
    io:format("RecordField ~p\n", [Value1]),
    istype(Line, Value1, RecordFieldType, Types, Records);
istype_record(Line, Value0, Record, [RecordFieldSpec | RecordFields], Types, Records) ->
    {RecordField, RecordFieldType} = RecordFieldSpec,
    Value1 = {record_field, Line, Value0, Record, {atom, Line, RecordField}},
    io:format("RecordField ~p\n", [Value1]),
    {op, Line, 'andalso',
        istype(Line, Value1, RecordFieldType, Types, Records),
        istype_record(Line, value, Record, RecordFields, Types, Records)}.

%%=============================================================================
%% parsing functions
%%=============================================================================
%% @doc Functions that are responsible for converting type specs and
%%      calls that represent type specs into the interlan type format.
%% @end
%% @doc If we fail to parse a type print an error and halt the build. 
%% @end
try_parse_type(Type) ->
    try
        parse_type(Type)
    catch
        Class:Error ->
            Stack = erlang:get_stacktrace(),
            io:format("Error when parsing ~p\n", [Type]),
            io:format("~p:~p\n", [Class, Error]),
            io:format("~p\n", [Stack]),
            halt(badarg)
    end.

%%==========================================================
%% parse_type
%%==========================================================
%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
%%======================================
%% any()
%%======================================
%% @doc Expect :: {type, _, any, []}
%%              | {call, _, {atom, _, any}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% none()
%%======================================
%% @doc Expect :: {type, _, none, []}
%%              | {call, _, {atom, _, none}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% pid()
%%======================================
%% @doc Expect :: {type, _, pid, []}
%%              | {call, _, {atom, _, pid}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% port()
%%======================================
%% @doc Expect :: {type, _, port, []}
%%              | {call, _, {atom, _, port}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% reference()
%%======================================
%% @doc Expect :: {type, _, reference, []}
%%              | {call, _, {atom, _, reference}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% [] - nil()
%%======================================
%% @doc Expect :: {nil, _}
%%              | {type, _, nil, []}
%%              | {call, _, {atom, _, nil}, []}
%%
%%      The type and call cases can be handled by the default handler.
%% @end
parse_type({nil, _}) ->
    {literal, nil, []};
%%======================================
%% Atom
%%======================================
%% atom()
%%==================
%% @doc Expect :: {type, _, atom, []}
%%              | {call, _, {atom, _, atom}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%==================
%% Erlang_Atom
%%==================
%% @doc Expect :: {atom, _, Erlang_Atom}
%%
%%      Erlang_Atoms need to be treated as literals.
%% @end
parse_type({atom, _, Atom}) ->
    {literal, atom, Atom};
%%======================================
%% Bitstring
%%======================================
%% @doc Expect :: <<>>
%%              | <<_:M>>
%%              | <<_:_*N>>
%%              | <<_:M, _:_*N>>
%%
%%      These patterns represent specific bitstrings formats.
%%      They need to be treated as literal values and should only
%%      be found within type specs.
%% @end
parse_type({type, _, binary, [{integer, _, M}, {integer, _, N}]}) ->
    {type, bitstring, {M, N}};
%%======================================
%% float()
%%======================================
%% @doc Expect :: {type, _, float, []}
%%              | {call, _, {atom, _, float}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% Fun
%%======================================
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
%%======================================
%% Integer
%%======================================
%% integer()
%%==================
%% @doc Expect :: {type, _, integer, []}
%%              | {call, _, {atom, _, integer}, []}
%%
%%      All cases can be handled by the default handler.
%% @end
%%=================
%% Erlang_Integer
%%=================
%% @doc Expect :: {integer, _, Erlang_Integer}
%%
%%      Erlang_Integers need to be treated as literals.
%% @end
parse_type({integer, _, Integer}) ->
    {literal, integer, Integer};
parse_type({op, _, '-', {integer, _, Integer}}) ->
    {literal, integer, -1 * Integer};
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
%% @doc Expect :: {type, _, range, [Erlang_Integer, Erlang_Integer]}
%%
%%      Ranges of Erlang_Integers.
%% @end
parse_type({type, _, range, [Low, High]}) ->
    {literal, _, LowerBound} = parse_type(Low),
    {literal, _, UpperBound} = parse_type(High),
    {type, range, {LowerBound, UpperBound}};
%%======================================
%% List
%%======================================
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
    {type, list, [maybe_empty, parse_type(Type), parse_type({type, 1, nil, []})]};
parse_type({type, _, maybe_improper_list, Types}) ->
    {type, list, [maybe_empty | lists:map(fun parse_type/1, Types)]};
parse_type({type, _, nonempty_improper_list, Types}) ->
    {type, list, [nonempty | lists:map(fun parse_type/1, Types)]};
parse_type({type, _, nonempty_list, [Type]}) ->
    {type, list, [nonempty, parse_type(Type), parse_type({type, 1, nil, []})]};
%%======================================
%% Map
%%======================================
%% @doc Expect :: {map, _, MapFields}           %% Literal
%%              | {type, _, map, any}           %% Any Map
%%              | {call, _, {atom, _, map}, []} %% Any Map
%%              | {type, _, map, []}            %% Empty Map
%%              | {type, _, map, MapFields}     %% Typed Map
%%
%%      MapFields :: list({type, _, map_field_assoc, Types})
%%                 | list({type, _, map_field_exact, Types})
%%      
%%      A call to map() is treated as any map.
%% @end
parse_type({map, _, _} = Map) ->
    {literal, map, erl_parse:normalise(Map)};
parse_type({type, _, map, any}) ->
    {type, map, any};
parse_type({call, _, {atom, _, map}, []}) ->
    {type, map, any};
parse_type({type, _, map, []}) ->
    {type, map, empty};
parse_type({type, _, map, MapFields}) ->
    {type, map, lists:map(fun parse_map_field/1, MapFields)};
%%======================================
%% Tuple
%%======================================
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
%%======================================
%% Union
%%======================================
%% @doc Expect :: {type, _, union, Types} %% Any Tuple
%% @end
parse_type({type, _, union, Types}) ->
    {type, union, lists:map(fun parse_type/1, Types)};
%%======================================
%% term()
%%======================================
%% @doc Expect :: {type, _, term, []}
%%              | {call, _, {atom, _, term}, []}
%%      Alias for any().
%%
%%      Calls are handled by the default call handler.
%% @end
parse_type({type, Line, term, []}) ->
    parse_type({type, Line, any, []});
%%======================================
%% binary()
%%======================================
%% @doc Expect :: {type, _, binary, []}
%%              | {call, _, {atom, _, binary}, []}
%%
%%      Alias for <<_:_*8>>.
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% bitstring()
%%======================================
%% @doc Expect :: {type, _, bitstring, []}
%%              | {call, _, {atom, _, bitstring}, []}
%%
%%      Alias for <<_:_*1>>.
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% boolean()
%%======================================
%% @doc Expect :: {type, _, boolean, []}
%%              | {call, _, {atom, _, boolean}, []}
%%
%%      Alias for 'true' | 'false'.
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% byte()
%%======================================
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
%%======================================
%% char()
%%======================================
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
%%======================================
%% nil()
%%======================================
%% @doc Alias for [].
%%      All cases handled by [] above.
%% @end
%%======================================
%% number()
%%======================================
%% @doc Expect :: {type, _, number, []}
%%              | {call, _, {atom, _, number}, []}
%%
%%      Alias for integer() | float().
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% list()
%%======================================
%% @doc Expect :: {type, _, list, []}
%%              | {call, _, {atom, _, list}, []}
%%
%%      Alias for list(any()).
%%
%%      Calls handled by List above.
%% @end
parse_type({type, Line, list, []}) ->
    parse_type({type, Line, list, [{type, Line, any, []}]});
%%======================================
%% maybe_improper_list()
%%======================================
%% @doc Expect :: {type, _, maybe_improper_list, []}
%%              | {call, _, {atom, _, maybe_improper_list}, []}
%%
%%      Handled by List above.
%% @end
%%======================================
%% nonempty_list()
%%======================================
%% @doc Expect :: {type, _, nonempty_list, []}
%%              | {call, _, {atom, _, nonempty_list}, []}
%%
%%      Alias for nonempty_list(any()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, nonempty_list, []}) ->
    parse_type({type, Line, nonempty_list, [{type, Line, any, []}]});
%%======================================
%% string()
%%======================================
%% @doc Expect :: {type, _, string, []}
%%              | {call, _, {atom, _, string}, []}
%%
%%      Alias for list(char()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, string, []}) ->
    parse_type({type, Line, list, [{type, Line, char, []}]});
%%======================================
%% nonempty_string()
%%======================================
%% @doc Expect :: {type, _, nonempty_string, []}
%%              | {call, _, {atom, _, nonempty_string}, []}
%%
%%      Alias for [char, ...], aka, nonempty_list(char()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, nonempty_string, []}) ->
    parse_type({type, Line, nonempty_list, [{type, Line, char, []}]});
%%======================================
%% iodata()
%%======================================
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
%%======================================
%% iolist()
%%======================================
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
%%======================================
%% function()
%%======================================
%% @doc Expect :: {type, _, function, []}
%%              | {call, _, {atom, _, function}, []}
%%
%%      Alias for fun().
%%      All cases can be handled by the default handler.
%% @end
%%======================================
%% module()
%%======================================
%% @doc Expect :: {type, _, module, []}
%%              | {call, _, {atom, _, module}, []}
%%
%%      Alias for atom().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, module, []}) ->
    parse_type({type, Line, atom, []});
%%======================================
%% mfa()
%%======================================
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
%%======================================
%% arity()
%%======================================
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
%%======================================
%% identifier()
%%======================================
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
%%======================================
%% node()
%%======================================
%% @doc Expect :: {type, _, node, []}
%%              | {call, _, {atom, _, node}, []}
%%
%%      Alias for atom().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, node, []}) ->
    parse_type({type, Line, atom, []});
%%======================================
%% timeout()
%%======================================
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
%%======================================
%% no_return()
%%======================================
%% @doc Expect :: {type, _, no_return, []}
%%              | {call, _, {atom, _, no_return}, []}
%%
%%      Alias for none().
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, Line, no_return, []}) ->
    parse_type({type, Line, none, []});
%%======================================
%% non_neg_integer()
%%======================================
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
%%======================================
%% pos_integer()
%%======================================
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
%%======================================
%% neg_integer()
%%======================================
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
%%======================================
%% nonempty_maybe_improper_list()
%%======================================
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
%%======================================
%% nonempty_improper_list()
%%======================================
%% @doc Expect :: {type, _, nonempty_improper_list, []}
%%              | {call, _, {atom, _, nonempty_improper_list}, []}
%%
%%      Handled in the List section above.
%% @end
%%======================================
%% nonempty_maybe_improper_list(Type1, Type2)
%%======================================
%% @doc Expect :: {type, _, nonempty_maybe_improper_list, [Type1, Type2]}
%%              | {call, _, {atom, _, nonempty_maybe_improper_list}, [Type1, Type2]}
%%
%%      Alias for nonempty_maybe_improper_list(any(), any()).
%%
%%      Calls handled by the default call handler.
%% @end
parse_type({type, _, nonempty_maybe_improper_list, Types}) ->
    {type, list, [nonempty | lists:map(fun parse_type/1, Types)]};
%%======================================
%% Record
%%======================================
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

%%======================================
%% Default call handler
%%======================================
parse_type({call, Line, {atom, _, Type}, TypeArgs}) ->
    parse_type({type, Line, Type, TypeArgs});
%%======================================
%% Default handler
%%======================================
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
