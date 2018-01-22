-module(istype_transform).
-export([parse_transform/2]).

%%====================================================================
%% parse_transform api
%%====================================================================
log_parse_type(_) -> ok.

parse_transform(Forms0, _Options) ->
    try
        file:write_file("istype_test.forms.erl", [io_lib:format("~p\n", [Forms0])]),
        {attribute, _, module, Module} = lists:keyfind(module, 3, Forms0),
        Records = forms:reduce(fun get_records/2, #{}, Forms0),
        Types = forms:reduce(fun get_types/2,
                             #{iolist => log_parse_type({type, 1, iolist, []})},
                             Forms0),

        Forms1 = add_default_records(Forms0, Records),
        forms:map(fun(Form) ->
                      do_transform(Module, Form, Types, Records)
                  end,
                  Forms1)
    catch
        Class:Error ->
            handle_error(Class, Error, erlang:get_stacktrace()),

            halt(1)
    end.

do_transform(_, {call, Line, {atom, _, istype}, [Value, Type]}, Types, Records) ->
    %% is_integer(Value) orelse is_boolean(Value) ...
    %%
    %% OR
    %%
    %% begin
    %%     __IsType_1 = expression(),
    %%     is_integer(__IsType_1) orelse is_boolean(__IsType_1) ...
    %% end
    optimize_istype(Line, Value, log_parse_type(Type), Types, Records);
do_transform(Module, {call, Line, {atom, _, totype}, [Value, Type0]}, Types, Records) ->
    %% try
    %%     __IsType_1 = istype_lib:totype(Value, TypeInfo),
    %%     asserttype(__IsType_1, type()),
    %%     __IsTYpe_1
    %% catch
    %%     error:{badmatch, false} ->
    %%         error({istype_conversion, type(), __IsType_1})
    %% end
    Type1 = log_parse_type(Type0),

    Type2 = case Type1 of
                {record, _, _} = Record ->
                    record_to_record_spec(Module, Record, Types, Records);
                _ ->
                    Type1
            end,

    %io:format("++++++++++++++++\nToType\n~p\n~p\n~p\n----------------\n", [Type0, Type1, Type2]),

    Converted = get_var(Line),
    {'try', Line,
        [{match, Line,
             Converted,
             {call, Line,
                 {remote, Line,
                     {atom, Line, istype_lib},
                     {atom, Line, totype}},
                 [Value,
                  erl_parse:abstract(Type2, [{line, Line}]),
                  erl_parse:abstract(Types, [{line, Line}]),
                  erl_parse:abstract(Records, [{line, Line}])]}},
         do_transform(Module,
                      {call, Line,
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
                                  erl_parse:abstract(Type2, [{line, Line}]),
                                  Value,
                                  {atom, Line, invalid_result}]}]}]}],
        []};
do_transform(Module, {call, Line, {atom, _, asserttype}, Args}, Types, Records) ->
    %% true = istype(Value, type())
    {match, Line,
        {atom, Line, true},
        do_transform(Module, {call, Line, {atom, Line, istype}, Args}, Types, Records)};
do_transform(_, Form, _, _) ->
    Form.

handle_error(Class, Error, Stack) ->
    erlang:Class({Class, Error, Stack}).

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
%% @doc Expect :: {type, bitstring, {M, N}}
%% @end
istype(Line, Value, {type, bitstring, {0, 0}}, _, _) ->
    {op, Line, '=:=',
            Value,
            {bin, Line, []}};

istype(Line, Value, {type, bitstring, {M, 0}}, _, _) ->
    {op, Line, 'andalso',
        {call, Line, {atom, Line, is_bitstring}, [Value]},
        {op, Line, '=:=',
            {call, Line, {atom, Line, bit_size}, [Value]},
            {integer, Line, M}}};

istype(Line, Value, {type, bitstring, {M, N}}, _, _) ->
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
    {call, Line, {atom, Line, is_function}, [Value]};
istype(Line, Value, {type, 'fun', _}, _, _) ->
    {call, Line, {atom, Line, is_function}, [Value]};
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
        erl_parse:abstract(Map, [{line, Line}])};
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
istype(Line, Value, {record_spec, Record, RecordInfo}, Types, Records) ->
    {Arity, _, _, FieldTypes} = RecordInfo,
    RecordFields = lists:filter(fun({_, FieldType}) ->
                                    not is_any(FieldType, Types)
                                end,
                                FieldTypes),
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
istype(Line, Value, {deep_type, _, _} = TypeSpec0, Types, Records) ->
    TypeSpec1 = setelement(1, TypeSpec0, type),
    {call, Line,
        {remote, Line,
            {atom, Line, istype_lib},
            {atom, Line, istype}},
        [Value,
         erl_parse:abstract(TypeSpec1, [{line, Line}]),
         erl_parse:abstract(Types, [{line, Line}]),
         erl_parse:abstract(Records, [{line, Line}])]};
%%=====================================
%% Literals
%%=====================================
%% @doc Expect :: {literal, Value}
%%
%%      Specific value comparison.
%% @end
istype(Line, Value, {literal, Literal}, _, _) ->
    {op, Line, '=:=', Value, Literal};
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
                       error({unknown_type, Type})
               end,
    istype(Line, Value, TypeSpec, Types, Records);
istype(Line, Value, {record, _, _} = Record, Types, Records) ->
    RecordSpec = try
                     record_to_record_spec(Record, Types, Records)
                 catch
                     Class:Error ->
                        error({Class, Error, erlang:get_stacktrace()})
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
    istype(Line, Value1, RecordFieldType, Types, Records);
istype_record(Line, Value0, Record, [RecordFieldSpec | RecordFields], Types, Records) ->
    {RecordField, RecordFieldType} = RecordFieldSpec,
    Value1 = {record_field, Line, Value0, Record, {atom, Line, RecordField}},
    {op, Line, 'andalso',
        istype(Line, Value1, RecordFieldType, Types, Records),
        istype_record(Line, Value0, Record, RecordFields, Types, Records)}.


%%====================================================================
%% default_records functions
%%====================================================================
%% default_records
%%==========================================================
add_default_records(Forms, Records) ->
    Last = element(2, hd(lists:reverse(Forms))),
    {Export, Fun} = default_records(Last, Records),
    maybe_add_default_records(Last, Export, Fun, Forms).

default_records(Line, Records) ->
    Export = {attribute, line, export, [{istype_default_records, 1}]},
    Clauses = maps:fold(fun(Record, _, Acc) ->
                            Clause = {clause, Line, [{atom, Line, Record}], [], [{record, Line, Record, []}]},
                            [Clause | Acc]
                        end,
                        [],
                        Records),

    case Clauses of
        [] ->
            {undefined, undefined};
        _ ->
            Fun = {function, Line, istype_default_records, 1, Clauses},
            {Export, Fun}
    end.

maybe_add_default_records(_, undefined, undefined, Forms) ->
    Forms;
maybe_add_default_records(Last, Export0, Fun, Forms0) ->
    {_, Forms1} = lists:foldl(fun({eof, _}, {false, Acc}) ->
                                     {false, Acc};
                                 (Form, {false, Acc}) ->
                                     {false, [Form | Acc]};
                                 (Form, {true, Acc}) ->
                                     case Form of
                                         {attribute, Line, export, _} ->
                                             Export1 = setelement(2, Export0, Line),
                                             {false, [Export1, Form | Acc]};
                                         _ ->
                                             {true, [Form | Acc]}
                                     end
                              end,
                              {true, []},
                              Forms0),
    lists:reverse([{eof, Last + 1}, Fun | Forms1]).

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
get_types({attribute, _, type, {Type, {remote_type, _, _} = TypeSpec, _} = X}, Acc) ->
  Acc;
get_types({attribute, _, type, {Type, TypeSpec, _} = X}, Acc) ->
  Acc#{Type => log_parse_type(TypeSpec)};
get_types(_, Acc) ->
  Acc.

%% @doc Function that generates a mapping of records to record specs.
%% @end
get_records({attribute, _, record, {Record, RecordFields}} = R, Acc) ->
    %io:format("Parsing Record\n~p\n", [R]),
    %{attribute,23,record,
    %    {record_a,
    %        [{typed_record_field,
    %             {record_field,23,{atom,23,a},{atom,23,atom}},
    %             {type,23,atom,[]}},
    %         {record_field,24,{atom,24,b},{atom,24,atom}},
    %         {typed_record_field,{record_field,25,{atom,25,c}},{type,25,atom,[]}},
    %         {record_field,26,{atom,26,d}},
    %         {typed_record_field,
    %             {record_field,27,{atom,27,e}},
    %             {type,27,union,[{type,27,atom,[]},{type,27,binary,[]}]}}]}}
    Acc;%Acc#{Record => parse_record_fields(RecordFields)};
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

record_to_record_spec(Module, Record, Types, Records) ->
    {record_spec, RecordLabel, RecordProperties} = record_to_record_spec(Record, Types, Records),
    {record_spec, RecordLabel, RecordProperties, Module}.

record_to_record_spec({record, Record, Overrides}, Types, Records) ->
    {record_spec, Record, override_record_spec(Overrides, Record, Types, Records)}.

%% @doc Fetches a record spec with the overrides applied.
%% @end
override_record_spec(Overrides, Record, _, Records) ->
    #{Record := {Arity, RecordFields, RecordTypes}} = Records,
    DefaultTypes = lists:zip(RecordFields, RecordTypes),

    %% Update types
    Overridden = lists:foldl(fun({K, V}, Acc) ->
                                 lists:keyreplace(K, 1, Acc, {K, V})
                             end,
                             DefaultTypes,
                             Overrides),

    {_, OverriddenTypes} = lists:unzip(Overridden),
    {Arity, RecordFields, OverriddenTypes, Overridden}.
