-module(istype_lib).
-export([totype/4, istype/4]).

%%==============================================================================
%% istype functions
%%==============================================================================
%% istype
%%==========================================================
istype(_, _, _, _) ->
    true.

%%==============================================================================
%% totype functions
%%==============================================================================
%% totype
%%==========================================================
totype(Value, Type, Types, Records) ->
    io:format("Convert ~p\nTo ~p\n", [Value, Type]),
    try
        do_totype(Value, Type, Types, Records)
    catch
        Class:Error ->
            Reason = {Class, Error},
            conversion_error(Value, Type, Reason)
    end.
%%==========================================================
%% do_totype
%%==========================================================
%% @doc Converts the given value to the given type.
%% @end
%%======================================
%% Internal Shorthand
%%======================================
do_totype(Value, list, Types, Records) ->
    do_totype(Value,
           {type, list, [maybe_empty,
                         {type, any, []},
                         {type, any, []}]},
           Types,
           Records);
do_totype(Value, map, Types, Records) ->
    do_totype(Value, {type, map, any}, Types, Records);
do_totype(Value, Type, Types, Records) when is_atom(Type) ->
    io:format("Shorthand ~p\n", [Type]),
    do_totype(Value, {type, Type, []}, Types, Records);
%%======================================
%% any()
%%======================================
do_totype(Value, {type, any, []}, _, _) ->
    Value;
%%======================================
%% none()
%%======================================
do_totype(Value, {type, none, []} = Type, _, _) ->
    conversion_error(Value, Type);
%%======================================
%% pid()
%%======================================
do_totype(Value, {type, pid, []}, _, _) when is_pid(Value) ->
    Value;
do_totype(Value, {type, pid, []}, _, _) when is_list(Value) ->
    list_to_pid(Value);
do_totype(Value, {type, pid, []}, Types, Records) when is_binary(Value) ->
    AsList = do_totype(Value, list, Types, Records),
    do_totype(AsList, pid, Types, Records);
%%======================================
%% port()
%%======================================
do_totype(Value, {type, port, []}, _, _) when is_port(Value) ->
    Value;
do_totype(Value, {type, port, []}, _, _) when is_list(Value) ->
    list_to_port(Value);
do_totype(Value, {type, port, []}, Types, Records) when is_binary(Value) ->
    AsList = do_totype(Value, list, Types, Records),
    do_totype(AsList, port, Types, Records);
%%======================================
%% reference()
%%======================================
do_totype(Value, {type, reference, []}, _, _) when is_reference(Value) ->
    Value;
do_totype(Value, {type, reference, []}, _, _) when is_list(Value) ->
    list_to_ref(Value);
do_totype(Value, {type, reference, []}, Types, Records) when is_binary(Value) ->
    AsList = do_totype(Value, list, Types, Records),
    do_totype(AsList, reference, Types, Records);
%%======================================
%% [] - nil()
%%======================================
do_totype(<<>>, {Class, nil, []}, _, _) when Class =:= type orelse
                                             Class =:= literal ->
    [];
do_totype([], {Class, nil, []}, _, _) when Class =:= type orelse
                                           Class =:= literal ->
    [];
%%======================================
%% Atom
%%======================================
%%==================
%% atom()
%%==================
do_totype(Value, {type, atom, []}, _, _) when is_atom(Value) ->
    Value;
do_totype(Value, {type, atom, []}, _, _) when is_binary(Value) ->
    binary_to_atom(Value, utf8);
do_totype(Value, {type, atom, []}, _, _) when is_list(Value) ->
    list_to_atom(Value);
%%==================
%% Erlang_Atom
%%==================
do_totype(Value, {literal, atom, Atom}, Types, Records) ->
    case do_totype(Value, atom, Types, Records) of
        Atom ->
            Atom;
        _ ->
            conversion_error(Value, {literal, atom, Atom})
    end;
%%======================================
%% Bitstring
%%======================================
%% @doc Bitstring literals and patterns
%% @end
%%======================================
%% float()
%%======================================
do_totype(Value, {type, float, []}, _, _) when is_float(Value) ->
    Value;
do_totype(Value, {type, float, []}, _, _) when is_integer(Value) ->
    1.0 * Value;
do_totype(Value, {type, float, []}, Types, Records) when is_binary(Value) orelse 
                                                         is_list(Value) ->
    AsNumber = do_totype(Value, number, Types, Records),
    do_totype(AsNumber, float, Types, Records);
%%======================================
%% Fun
%%======================================
do_totype(Value, {type, 'fun', _} = Type, _, _) ->
    conversion_error(Value, Type);
%%======================================
%% Integer
%%======================================
%%==================
%% integer()
%%==================
do_totype(Value, {type, integer, []}, _, _) when is_float(Value) ->
    trunc(Value);
do_totype(Value, {type, integer, []}, _, _) when is_integer(Value) ->
    Value;
do_totype(Value, {type, integer, []}, Types, Records) when is_binary(Value) orelse 
                                                           is_list(Value) ->
    AsNumber = do_totype(Value, number, Types, Records),
    do_totype(AsNumber, integer, Types, Records);
%%==================
%% Erlang_Integer
%%==================
do_totype(Value, {literal, integer, Integer}, Types, Records) ->
    case do_totype(Value, integer, Types, Records) of
        Integer ->
            Integer;
        _ ->
            conversion_error(Value, {literal, integer, Integer})
    end;
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
do_totype(Value, {type, range, {LowerBound, UpperBound}} = Type, Types, Records) ->
    AsInteger = do_totype(Value, integer, Types, Records),
    case AsInteger of
        _ when LowerBound =:= undefined andalso
               AsInteger =< UpperBound ->
            AsInteger;
        _ when UpperBound =:= undefined andalso
               AsInteger >= LowerBound ->
            AsInteger;
        _ when AsInteger >= LowerBound andalso
               AsInteger =< UpperBound ->
            AsInteger;
        _ ->
            conversion_error(Value, Type)
    end;
%%======================================
%% List
%%======================================
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_list(Value) ->
    Value;
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_pid(Value) ->
    pid_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_port(Value) ->
    port_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_reference(Value) ->
    ref_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_atom(Value) ->
    atom_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_bitstring(Value) ->
    bitstring_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_binary(Value) ->
    binary_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_float(Value) ->
    float_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_integer(Value) ->
    integer_to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, _, _) when is_map(Value) ->
    maps:to_list(Value);
do_totype(Value, {type, list, [maybe_empty, {type, any, []}, {type, any, []}]}, Types, Records) when is_tuple(Value) ->
    case maps:get(element(1, Value), Records, undefined) of
        {Arity, Fields, _} when size(Value) =:= Arity ->
            AsRecord = do_totype(Value, {record, element(1, Value), []}, Types, Records),
            lists:zip(Fields, tl(tuple_to_list(AsRecord)));
        _ ->
            tuple_to_list(Value)
    end;
do_totype(Value, {type, list, [Empty, ValueType, TerminatorType]} = Type, Types, Records) when is_list(Value) ->
    try
        do_totype_list(Value, Empty, ValueType, TerminatorType, Types, Records)
    catch
        error:{istype_conversion, _, _} ->
            conversion_error(Value, Type)
    end;
do_totype(Value, {type, list, _} = Type, Types, Records) ->
    do_totype(do_totype(Value, list, Types, Records), Type, Types, Records);
%%======================================
%% Map
%%======================================
do_totype(#{}, {type, map, empty}, _, _) ->
    #{};
do_totype([], {type, map, empty}, _, _) ->
    #{};
do_totype(<<>>, {type, map, empty}, _, _) ->
    #{};
do_totype(Value, {type, map, any}, _, _) when is_map(Value) ->
    io:format("Map Map ~p\n", [any]),
    Value;
do_totype(Value, {type, map, any}, _, _) when is_list(Value) ->
    io:format("Map List ~p\n", [any]),
    try
        maps:from_list(Value)
    catch
        _:_ ->
            conversion_error(Value, {type, map, any})
    end;
do_totype(Value, {type, map, any}, Types, Records) when is_tuple(Value) ->
    io:format("Map Tuple ~p\n", [any]),
    case maps:get(element(1, Value), Records, undefined) of
        {Arity, Fields, _} when size(Value) =:= Arity ->
            do_totype(lists:zip(Fields, tl(tuple_to_list(Value))), map, Types, Records);
        _ ->
            conversion_error(Value, {type, map, any})
    end;
do_totype(Value, {type, map, any} = Type, _, _) ->
    conversion_error(Value, Type);
do_totype(Value, {type, map, _} = Type, Types, Records) ->
    io:format("Map ~p\n", [Type]),
    AsMap = do_totype(Value, map, Types, Records),
    do_totype_map(AsMap, Type, Types, Records);
%%======================================
%% Tuple
%%======================================
do_totype(Value, {type, tuple, any}, _, _) when is_tuple(Value) ->
    Value;
do_totype(Value, {type, tuple, any}, _, _) when is_list(Value) ->
    tuple_to_list(Value);
do_totype({}, {type, tuple, empty}, _, _) ->
    {};
do_totype(<<>>, {type, tuple, empty}, _, _) ->
    {};
do_totype([], {type, tuple, empty}, _, _) ->
    {};
do_totype(Value, {type, tuple, _} = Type, Types, Records) when is_tuple(Value) ->
    do_totype(tuple_to_list(Value), Type, Types, Records);
do_totype(Value, {type, tuple, FieldTypes} = Type, Types, Records) when is_list(Value) ->
    TupleSize = length(FieldTypes),
    ValueSize = length(Value),

    case TupleSize of
        ValueSize ->
            ok;
        _ ->
            conversion_error(Value, Type)
    end,

    Converted = lists:map(fun({FieldValue, FieldType}) ->
                              do_totype(FieldValue, FieldType, Types, Records)
                          end,
                          lists:zip(Value, FieldTypes)),
    list_to_tuple(Converted);
%%======================================
%% Union
%%======================================
do_totype(Value, {type, union, []} = Type, _, _) ->
    conversion_error(Value, Type);
do_totype(Value, {type, union, [Type | UnionTypes]}, Types, Records) ->
    try
        do_totype(Value, Type, Types, Records)
    catch
        error:{} ->
            do_totype(Value, {type, union, UnionTypes}, Types, Records)
    end;
%%======================================
%% term()
%%======================================
%% @doc Converted to any() during parse
%% @end
%%======================================
%% binary()
%%======================================
do_totype(Value, {type, binary, []}, _, _) when is_binary(Value) ->
    Value;
do_totype(Value, {type, binary, []}, Types, Records) when is_pid(Value) orelse
                                                          is_port(Value) orelse
                                                          is_reference(Value) orelse
                                                          is_function(Value) ->
    list_to_binary(do_totype(Value, list, Types, Records));
do_totype(Value, {type, binary, []}, _, _) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
do_totype(Value, {type, binary, []}, _, _) when is_float(Value) ->
    float_to_binary(Value);
do_totype(Value, {type, binary, []}, _, _) when is_integer(Value) ->
    integer_to_binary(Value);
do_totype(Value, {type, binary, []}, _, _) when is_list(Value) ->
    list_to_binary(Value);
%%======================================
%% bitstring()
%%======================================
do_totype(Value, {type, bitstring, []}, _, _) when is_bitstring(Value) orelse
                                                   is_binary(Value) ->
    Value;
do_totype(Value, {type, bitstring, []}, Types, Records) when is_pid(Value) orelse
                                                             is_port(Value) orelse
                                                             is_reference(Value) orelse
                                                             is_atom(Value) orelse
                                                             is_float(Value) orelse
                                                             is_integer(Value) orelse
                                                             is_function(Value) ->
    AsList = do_totype(Value, list, Types, Records),
    do_totype(AsList, bitstring, Types, Records);
do_totype(Value, {type, bitstring, []}, _, _) when is_list(Value) ->
    list_to_bitstring(Value);
%%======================================
%% boolean()
%%======================================
do_totype(Value, {type, boolean, []} = Type, Types, Records) ->
    AsAtom = do_totype(Value, atom, Types, Records),
    case AsAtom of
        true ->
            true;
        false ->
            false;
        _ ->
            conversion_error(Value, Type)
    end;
%%======================================
%% byte()
%%======================================
%% @doc Converted to range() during parse
%% @end
%%======================================
%% char()
%%======================================
%% @doc Converted to range() during parse
%% @end
%%======================================
%% nil()
%%======================================
%% @doc Handled by [] above
%% @end
%%======================================
%% number()
%%======================================
do_totype(Value, {type, number, []}, _, _) when is_float(Value) orelse
                                                is_integer(Value) ->
    Value;
do_totype(Value, {type, number, []}, Types, Records) when is_list(Value) ->
    AsBinary = do_totype(Value, binary, Types, Records),
    do_totype(AsBinary, number, Types, Records);
do_totype(Value, {type, number, []}, _, _) when is_binary(Value) ->
    try
        binary_to_integer(Value)
    catch
        _:_ ->
            binary_to_float(Value)
    end;
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
%% @doc Handled by Union
%% @end
%%======================================
%% arity()
%%======================================
%% @doc Converted to range() during parse
%% @end
%%======================================
%% identifier()
%%======================================
%% @doc Handled by Union
%% @end
%%======================================
%% node()
%%======================================
%% @doc Handled by atom()
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
%% @doc Converted to range() during parse
%% @end
%%======================================
%% pos_integer()
%%======================================
%% @doc Converted to range() during parse
%% @end
%%======================================
%% neg_integer()
%%======================================
%% @doc Converted to range() during parse
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
%%======================================
%% Record
%%======================================
%do_totype(Value, {record, _, _} = Record, Types, Records) when is_list(Value) ->
%    do_totype(maps:from_list(Value), Record, Types, Records);
%do_totype(Value, {record, Record, Overrides}, Types, Records) when is_map(Value) ->
%    #{Record := {Arity, Fields, FieldTypes}} = Records,


%%======================================
%% Custom Type
%%======================================
do_totype(Value, {type, TypeName, []}, Types, Records) ->
    #{TypeName := Type} = Types,
    do_totype(Value, Type, Types, Records);
%%======================================
%% No conversion
%%======================================
do_totype(Value, Type, _, _) ->
    conversion_error(Value, Type).

%%==========================================================
%% do_totype_list
%%==========================================================
%% @doc Convert into the specific list typing.
%% @end
do_totype_list(Value, Empty, ValueType, TerminatorType, Types, Records) ->
    do_totype_list(Value, Empty, ValueType, TerminatorType, Types, Records, []).
%% @doc Nonempty with an empty input is invalid.
%% @end
do_totype_list([], nonempty, ValueType, TerminatorType, _, _, _) ->
    conversion_error([], {type, list, [nonempty, ValueType, TerminatorType]});
%% @doc Maybe empty with an empty input is ok if [] is a valid terminator type.
%% @end
do_totype_list([], maybe_empty, _, TerminatorType, Types, Records, _) ->
    do_totype([], TerminatorType, Types, Records);
%% @doc When we see a nil tail, validate that nil is a valid tail,
%%      add to the Acc, and reverse the result.
%% @end
do_totype_list([Hd | []], _, ValueType, TerminatorType, Types, Records, Acc) ->
    _ = do_totype([], TerminatorType, Types, Records),
    lists:reverse([do_totype(Hd, ValueType, Types, Records) | Acc]);
%% @doc When tail is a list convert the value and recurse.
%% @end
do_totype_list([Hd | Tl], Empty, ValueType, TerminatorType, Types, Records, Acc0) when is_list(Tl) ->
    Acc1 = [do_totype(Hd, ValueType, Types, Records) | Acc0],
    do_totype_list(Tl, Empty, ValueType, TerminatorType, Types, Records, Acc1);
%% @doc When tail is not a list then it's the terminator of an improper list.
%%      Convert the value and terminator then generate an improper list.
%% @end
do_totype_list([Hd0 | Tl0], _, ValueType, TerminatorType, Types, Records, Acc) ->
    Hd1 = lists:reverse([do_totype(Hd0, ValueType, Types, Records) | Acc]),
    Tl1 = do_totype(Tl0, TerminatorType, Types, Records),
    make_improper_list(Hd1, Tl1).

make_improper_list([], Terminator) ->
    Terminator;
make_improper_list([Hd | Tl], Terminator) ->
    [Hd | make_improper_list(Tl, Terminator)].

%%==========================================================
%% do_totype_map
%%==========================================================
do_totype_map(Value0, {_, _, MapFields} = Type, Types, Records) ->
    %% Get the field types that are required
    Required0 = lists:filtermap(fun({require, KeyType, ValueType}) ->
                                       {true, {KeyType, ValueType}};
                                   (_) ->
                                       false
                                end,
                                MapFields),

    %% Generate a new map of converted types while reducing the set of required fields.
    {Required1, Value1} = maps:fold(fun(K0, V0, {RequiredAcc, ValueAcc}) ->
                                        {K1, V1, Assoc} = do_totype_map_assoc(K0, V0, MapFields, Types, Records),
                                        {RequiredAcc -- [Assoc], ValueAcc#{K1 => V1}}
                                    end,
                                    {Required0, #{}},
                                    Value0),
    %% If no required fields remain we succeeded.
    case Required1 of
        [] ->
            Value1;
        _ ->
            conversion_error(Value0, Type)
    end.

do_totype_map_assoc(Key, Value, [], _, _) ->
    conversion_error({Key, Value}, map_assoc);
do_totype_map_assoc(Key, Value, [AssocType | AssocTypes], Types, Records) ->
    try
        {_, KeyType, ValueType} = AssocType,
        ConvertedKey = do_totype(Key, KeyType, Types, Records),
        ConvertedValue = do_totype(Value, ValueType, Types, Records),
        {ConvertedKey, ConvertedValue, AssocType}
    catch
        _:_ ->
            do_totype_map_assoc(Key, Value, AssocTypes, Types, Records)
    end.
%%==========================================================
%% conversion_error
%%==========================================================
conversion_error(Value, Type) ->
    conversion_error(Value, Type, undefined).

conversion_error(Value, Type, Reason) ->
    error({istype_conversion, Value, Type, Reason}).
