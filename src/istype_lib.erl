-module(istype_lib).
-export([istype/2, istype/4, totype/2, totype/4]).

%%==============================================================================
%% istype functions
%%==============================================================================
%% istype
%%==========================================================
istype(Value, Type) ->
    istype(Value, Type, #{}, #{}).

istype(Value, Type, Types, Records) ->
    do_istype(Value, Type, Types, Records).
%%==========================================================
%% do_istype
%%==========================================================
%% @doc Validates the given value is the given type.
%% @end
%%======================================
%% ShortHand
%%======================================
do_istype(Value, Type, Types, Records) when is_atom(Type) ->
    shorthand(Value, Type, Types, Records, fun do_istype/4);
%%======================================
%% Literals
%%======================================
do_istype(Value, {literal, _, Literal}, _, _) ->
    Value =:= Literal;
%%======================================
%% any()
%%======================================
do_istype(_, {type, any, []}, _, _) ->
    true;
%%======================================
%% none()
%%======================================
do_istype(_, {type, none, []}, _, _) ->
    false;
%%======================================
%% pid()
%%======================================
do_istype(Value, {type, pid, []}, _, _) ->
    is_pid(Value);
%%======================================
%% port()
%%======================================
do_istype(Value, {type, port, []}, _, _) ->
    is_port(Value);
%%======================================
%% reference()
%%======================================
do_istype(Value, {type, reference, []}, _, _) ->
    is_reference(Value);
%%======================================
%% [] - nil()
%%======================================
do_istype(Value, {type, nil, []}, _, _) ->
    Value =:= [];
%%======================================
%% Atom
%%======================================
%% atom()
%%==================
do_istype(Value, {type, atom, []}, _, _) ->
    is_atom(Value);
%%==================
%% Erlang_Atom
%%==================
%% @doc Handled by literal handler
%% @end
%%======================================
%% Bitstring
%%======================================
do_istype(Value, {type, bitstring, Format}, _, _) ->
    is_valid_bitstring(Value, Format);
%%======================================
%% float()
%%======================================
do_istype(Value, {type, float, []}, _, _) ->
    is_float(Value);
%%======================================
%% Fun
%%======================================
do_istype(Value, {type, 'fun', []}, _, _) ->
    is_function(Value);
%%======================================
%% Integer
%%======================================
%% integer()
%%==================
do_istype(Value, {type, integer, []}, _, _) ->
    is_integer(Value);
%%==================
%% Erlang_Integer
%%==================
%% @doc Handled by the default handler
%% @end
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
do_istype(Value, {type, range, {LowerBound, UpperBound}}, _, _) when is_integer(Value) ->
    case Value of
        _ when LowerBound =:= undefined andalso Value =< UpperBound ->
            true;
        _ when UpperBound =:= undefined andalso Value >= LowerBound ->
            true;
        _ when Value >= LowerBound andalso Value =< UpperBound ->
            true;
        _ ->
            false
    end;
%%======================================
%% List
%%======================================
do_istype(Value, {type, list, any}, _, _) ->
    is_list(Value);
do_istype(Value, {type, list, TypeSpec}, Types, Records) when is_list(Value) ->
    {Empty, ValueType, TerminatorType} = TypeSpec,
    do_istype_list(Value, Empty, ValueType, TerminatorType, Types, Records);
%%======================================
%% Map
%%======================================
do_istype(Value, {type, map, any}, _, _) ->
    is_map(Value);
do_istype(Value, {type, map, _} = Type, Types, Records) when is_map(Value) ->
    do_istype_map(Value, Type, Types, Records);
%%======================================
%% Tuple
%%======================================
do_istype(Value, {type, tuple, any}, _, _) ->
    is_tuple(Value);
do_istype(Value, {type, tuple, empty}, _, _) ->
    Value =:= {};
do_istype(Value, {type, tuple, _} = Type, Types, Records) when is_tuple(Value) ->
    do_istype_tuple(Value, Type, Types, Records);
%%======================================
%% Union
%%======================================
do_istype(_, {type, union, []}, _, _) ->
    false;
do_istype(Value, {type, union, [Type | UnionTypes]}, Types, Records) ->
    case do_istype(Value, Type, Types, Records) of
        true ->
            true;
        false ->
            do_istype(Value, {type, union, UnionTypes}, Types, Records)
    end;
%%======================================
%% term()
%%======================================
%% @doc Converted to any() during parse.
%% @end
%%======================================
%% binary()
%%======================================
do_istype(Value, {type, binary, []}, _, _) ->
    is_binary(Value);
%%======================================
%% bitstring()
%%======================================
%% @doc Handled by Bitstring.
%% @end
%%======================================
%% boolean()
%%======================================
do_istype(Value, {type, boolean, []}, _, _) ->
    is_boolean(Value);
%%======================================
%% byte()
%%======================================
%% @doc Converted to range() during parse.
%% @end
%%======================================
%% char()
%%======================================
%% @doc Converted to range() during parse.
%% @end
%%======================================
%% nil()
%%======================================
%% @doc Handled by []
%% @end
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
%% @doc Handled by Fun
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
%% @doc Handled by range()
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
%% @doc Handled by range()
%% @end
%%======================================
%% pos_integer()
%%======================================
%% @doc Handled by range()
%% @end
%%======================================
%% neg_integer()
%%======================================
%% @doc Handled by range()
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

%%======================================
%% Custom
%%======================================
%% @doc Handled by List
%% @end
do_istype(Value, {type, TypeName, []}, Types, Records) ->
    #{TypeName := Type} = Types,
    do_istype(Value, Type, Types, Records);

%%======================================
%% Default handler
%%======================================
do_istype(_, _, _, _) ->
    false.

%%==========================================================
%% do_istype_list()
%%==========================================================
do_istype_list([], nonempty, _, _, _, _) ->
    false;
do_istype_list([], maybe_empty, _, _, _, _) ->
    true;
do_istype_list([Value | []], _, ValueType, TerminatorType, Types, Records) ->
    do_istype(Value, ValueType, Types, Records) andalso
    do_istype([], TerminatorType, Types, Records);
do_istype_list([Value | Values], Empty, ValueType, TerminatorType, Types, Records) when is_list(Values) ->
    do_istype(Value, ValueType, Types, Records) andalso
    do_istype_list(Values, Empty, ValueType, TerminatorType, Types, Records);
do_istype_list([Value | Terminator], _, ValueType, TerminatorType, Types, Records) ->
    do_istype(Value, ValueType, Types, Records) andalso
    do_istype(Terminator, TerminatorType, Types, Records).

%%==========================================================
%% do_istype_map()
%%==========================================================
do_istype_map(Value, {type, map, MapFields}, Types, Records) ->
    %% Get the field types that are required
    Required0 = map_fields_required(MapFields),

    %% Generate a new map of converted types while reducing the set of required fields.
    Results = maps:fold(fun(K0, V0, {RequiredAcc, true}) ->
                                case do_istype_map_assoc(K0, V0, MapFields, Types, Records) of
                                    {true, Assoc} ->
                                        {RequiredAcc -- [Assoc], true};
                                    false ->
                                        {RequiredAcc, false}
                                end;
                           (_, _, False) ->
                               False
                        end,
                        {Required0, true},
                        Value),

    case Results of
        {[], true} ->
            true;
        _ ->
            false
    end.

do_istype_map_assoc(_, _, [], _, _) ->
    false;
do_istype_map_assoc(Key, Value, [AssocType | AssocTypes], Types, Records) ->
    {_, KeyType, ValueType} = AssocType,
    Result = do_istype(Key, KeyType, Types, Records) andalso
             do_istype(Value, ValueType, Types, Records),
    case Result of
        true ->
            {true, {KeyType, ValueType}};
        false ->
            do_istype_map_assoc(Key, Value, AssocTypes, Types, Records)
    end.

%%==========================================================
%% do_istype_tuple()
%%==========================================================
do_istype_tuple(Value, {type, tuple, TupleTypes}, Types, Records) ->
    case length(TupleTypes) of
        X when X =:= size(Value) ->
            do_istype_tuple(Value, TupleTypes, Types, Records, true, 1);
        _ ->
            false
    end.

do_istype_tuple(_, _, _, _, false, _) ->
    false;
do_istype_tuple(_, [], _, _, _, _) ->
    false;
do_istype_tuple(Value, [TupleType | TupleTypes], Types, Records, _, Index) ->
    do_istype_tuple(Value,
                    TupleTypes,
                    Types,
                    Records,
                    do_istype(element(Index, Value), TupleType, Types, Records),
                    Index + 1).

%%==============================================================================
%% totype functions
%%==============================================================================
%% totype()
%%==========================================================
totype(Value, Type) ->
    totype(Value, Type, #{}, #{}).

totype(Value, Type, Types, Records) ->
    try
        io:format("Do convert ~p ~p\n", [Value, Type]),
        do_totype(Value, Type, Types, Records)
    catch
        error:{conversion_error, V1, T1, Reason} ->
            conversion_error(Value, Type, {V1, T1, Reason});
        Class:Error ->
            Reason = {Class, Error, erlang:get_stacktrace()},
            conversion_error(Value, Type, Reason)
    end.

%%==========================================================
%% do_totype()
%%==========================================================
%% @doc Converts the given value to the given type.
%% @end
%%======================================
%% ShortHand
%%======================================
do_totype(Value, Type, Types, Records) when is_atom(Type) ->
    shorthand(Value, Type, Types, Records, fun do_totype/4);
%%======================================
%% Literals
%%======================================
do_totype(Value, {literal, _, _} = Literal, Types, Records) ->
    do_totype_literal(Value, Literal, Types, Records);
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
%% @doc Handled by the literal handler
%% @end
%%======================================
%% Bitstring
%%======================================
%% @doc Bitstring literals and patterns
%% @end
do_totype(<<>>, {type, bitstring, {0, 0}}, _, _) ->
    <<>>;
do_totype([], {type, bitstring, {0, 0}}, _, _) ->
    <<>>;
do_totype(Value, {type, bitstring, {_, _} = Format}, _, _) when is_bitstring(Value) ->
    case is_valid_bitstring(Value, Format) of
        true ->
            Value;
        _ ->
            conversion_error(Value, {type, bitstring, Format}, bitstring_length)
    end;
do_totype(Value, {type, bitstring, _} = Type, Types, Records) when is_atom(Value) ->
    AsBinary = do_totype(Value, binary, Types, Records),
    do_totype(AsBinary, Type, Types, Records);
%% @doc Even though we support binary -> bitstring there is no need to do any work
%%      as binary is a subset of bitstring.
%% @end
%do_totype(Value, {type, bitstring, _} = Type, Types, Records) when is_binary(Value) ->
%    AsBinary = do_totype(Value, binary, Types, Records),
%    do_totype(AsBinary, Type, Types, Records);
do_totype(Value, {type, bitstring, _} = Type, Types, Records) when is_list(Value) ->
    AsBinary = do_totype(Value, binary, Types, Records),
    do_totype(AsBinary, Type, Types, Records);
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
%% @doc Handled by the literal handler
%% @end
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
do_totype(Value, {type, list, any}, _, _) when is_list(Value) ->
    Value;
do_totype(Value, {type, list, any}, _, _) when is_pid(Value) ->
    pid_to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_port(Value) ->
    port_to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_reference(Value) ->
    ref_to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_atom(Value) ->
    atom_to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_bitstring(Value) ->
    bitstring_to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_binary(Value) ->
    binary_to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_float(Value) ->
    float_to_list(Value, [{decimals, 20}, compact]);
do_totype(Value, {type, list, any}, _, _) when is_integer(Value) ->
    integer_to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_map(Value) ->
    maps:to_list(Value);
do_totype(Value, {type, list, any}, _, _) when is_function(Value) ->
    erlang:fun_to_list(Value);
do_totype(Value, {type, list, {Empty, _, _}} = Type, _, _) when is_tuple(Value) andalso 0 =:= size(Value) ->
    case Empty of
        maybe_empty ->
            [];
        nonempty ->
            conversion_error(Value, Type, empty_tuple)
    end;
do_totype(Value, {type, list, any}, Types, Records) when is_tuple(Value) ->
    case maps:get(element(1, Value), Records, undefined) of
        {Arity, Fields, _} when size(Value) =:= Arity ->
            AsRecord = do_totype(Value, {record, element(1, Value), []}, Types, Records),
            lists:zip(Fields, tl(tuple_to_list(AsRecord)));
        _ ->
            tuple_to_list(Value)
    end;
do_totype(Value, {type, list, {Empty, ValueType, TerminatorType}} = Type, Types, Records) ->
    try
        AsList = do_totype(Value, list, Types, Records),
        do_totype_list(AsList, Empty, ValueType, TerminatorType, Types, Records)
    catch
        error:{istype_conversion, ErrorValue, ErrorType, ErrorReason} ->
            conversion_error(Value, Type, {ErrorValue, ErrorType, ErrorReason});
        Class:Error ->
            Reason = {Class, Error, erlang:get_stacktrace()},
            conversion_error(Value, Type, Reason)
    end;
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
    Value;
do_totype(Value, {type, map, any}, _, _) when is_list(Value) ->
    try
        maps:from_list(Value)
    catch
        error:{istype_conversion, ErrorValue, ErrorType, ErrorReason} ->
            conversion_error(Value, {type, map, any}, {ErrorValue, ErrorType, ErrorReason});
        Class:Error ->
            Reason = {Class, Error, erlang:get_stacktrace()},
            conversion_error(Value, {type, map, any}, Reason)
    end;
do_totype(Value, {type, map, any}, Types, Records) when is_tuple(Value) ->
    case maps:get(element(1, Value), Records, undefined) of
        {Arity, Fields, _} when size(Value) =:= Arity ->
            do_totype(lists:zip(Fields, tl(tuple_to_list(Value))), map, Types, Records);
        _ ->
            conversion_error(Value, {type, map, any})
    end;
do_totype(Value, {type, map, any} = Type, _, _) ->
    conversion_error(Value, Type);
do_totype(Value, {type, map, _} = Type, Types, Records) ->
    AsMap = do_totype(Value, map, Types, Records),
    do_totype_map(AsMap, Type, Types, Records);
%%======================================
%% Tuple
%%======================================
do_totype(Value, {type, tuple, any}, _, _) when is_tuple(Value) ->
    Value;
do_totype(Value, {type, tuple, any}, _, _) when is_list(Value) ->
    list_to_tuple(Value);
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
        _:_ ->
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
    float_to_binary(Value, [{decimals, 20}, compact]);
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
do_totype(Value, {record_spec, _, _, _} = Record, Types, Records) when is_list(Value) ->
    do_totype(maps:from_list(Value), Record, Types, Records);
do_totype(Value, {record_spec, Record, {_, _, _, FieldTypes}, Module}, Types, Records) when is_map(Value) ->
    Default = Module:istype_default_records(Record),
    io:format("RecordSpec\n", []),
    io:format("Default ~p\n", [Default]),
    AtomMap = maps:fold(fun(K0, V, Acc) ->
                            K1 = do_totype(K0, atom, Types, Records),
                            Acc#{K1 => V}
                        end,
                        #{},
                        Value),
    io:format("AtomMap ~p\n", [AtomMap]),
    {_, Result} = lists:foldl(fun({F, T}, {Index, Acc}) ->
                                  case maps:get(F, AtomMap, undefined) of
                                      undefined ->
                                          {Index + 1, Acc};
                                      FieldValue ->
                                          io:format("Converting ~p ~p\n", [FieldValue, T]),
                                          {Index + 1, setelement(Index, Acc, do_totype(FieldValue, T, Types, Records))}
                                  end
                              end,
                              {2, Default},
                              FieldTypes),
    io:format("Generated Record\n~p\n~p\n", [Default, Result]),
    Result;

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
%% do_totype_literal
%%==========================================================
%% @doc Convert into the specific list typing.
%% @end
do_totype_literal(Value, {literal, Type, LiteralValue} = Literal, Types, Records) ->
    try
        case do_totype(Value, Type, Types, Records) of
            LiteralValue ->
                LiteralValue;
            _ ->
                conversion_error(Value, Literal)
        end
    catch
        error:{istype_conversion, ErrorValue, ErrorType, ErrorReason} ->
            conversion_error(Value, Literal, {ErrorValue, ErrorType, ErrorReason});
        Class:Error ->
            Reason = {Class, Error, erlang:get_stacktrace()},
            conversion_error(Value, Literal, Reason)
    end.

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
do_totype_list([], maybe_empty, ValueType, TerminatorType, Types, Records, _) ->
    case do_istype([], TerminatorType, Types, Records) of
        true ->
            [];
        false ->
            conversion_error([], {type, list, {maybe_empty, ValueType, TerminatorType}})
    end;
%% @doc When we see a nil tail, validate that nil is a valid tail,
%%      add to the Acc, and reverse the result.
%% @end
do_totype_list([Hd | []], _, ValueType, TerminatorType, Types, Records, Acc) ->
    case istype([], TerminatorType, Types, Records) of
        true ->
            lists:reverse([do_totype(Hd, ValueType, Types, Records) | Acc]);
        false ->
            conversion_error([], TerminatorType)
    end;
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
    Required0 = map_fields_required(MapFields),

    %% Generate a new map of converted types while reducing the set of required fields.
    {Required1, Value1} = maps:fold(fun(K0, V0, {RequiredAcc, ValueAcc}) ->
                                        case do_istype_map_assoc(K0, V0, MapFields, Types, Records) of
                                            {true, Assoc} ->
                                                {RequiredAcc -- [Assoc], ValueAcc#{K0 => V0}};
                                            false ->
                                                {K1, V1, Assoc} = do_totype_map_assoc(K0, V0, MapFields, Types, Records),
                                                {RequiredAcc -- [Assoc], ValueAcc#{K1 => V1}}
                                        end
                                    end,
                                    {Required0, #{}},
                                    Value0),
    %% If no required fields remain we succeeded.
    case Required1 of
        [] ->
            Value1;
        _ ->
            conversion_error(Value0, Type, {required_remaining, Required1})
    end.

do_totype_map_assoc(Key, Value, [], _, _) ->
    conversion_error({Key, Value}, map_assoc);
do_totype_map_assoc(Key, Value, [AssocType | AssocTypes], Types, Records) ->
    try
        {_, KeyType, ValueType} = AssocType,
        ConvertedKey = do_totype(Key, KeyType, Types, Records),
        ConvertedValue = do_totype(Value, ValueType, Types, Records),
        {ConvertedKey, ConvertedValue, {KeyType, ValueType}}
    catch
        _:_ ->
            do_totype_map_assoc(Key, Value, AssocTypes, Types, Records)
    end.

%%==============================================================================
%% Common
%%==============================================================================
%% validate bitstring length
%%==========================================================
is_valid_bitstring(Value, {M, N}) when is_bitstring(Value) ->
    case bit_size(Value) of
        X when N =:= 0 andalso
               X =:= M ->
            true;
        X when X rem N =:= M ->
            true;
        _ ->
            false
    end;
is_valid_bitstring(Value, []) when is_bitstring(Value) ->
    true;
is_valid_bitstring(_, _) ->
    false.

%%==========================================================
%% map_fields_required()
%%==========================================================
map_fields_required(MapFields) ->
    lists:filtermap(fun({require, KeyType, ValueType}) ->
                           {true, {KeyType, ValueType}};
                       (_) ->
                           false
                    end,
                    MapFields).
%%==========================================================
%% shorthand()
%%==========================================================
shorthand(Value, list, Types, Records, Action) ->
    Action(Value, {type, list, any}, Types, Records);
shorthand(Value, map, Types, Records, Action) ->
    Action(Value, {type, map, any}, Types, Records);
shorthand(Value, Type, Types, Records, Action) ->
    Action(Value, {type, Type, []}, Types, Records).

%%==============================================================================
%% Errors
%%==============================================================================
%% conversion_error
%%==========================================================
conversion_error(Value, Type) ->
    conversion_error(Value, Type, undefined).

conversion_error(Value, Type, Reason) ->
    error({istype_conversion, Value, Type, Reason}).
