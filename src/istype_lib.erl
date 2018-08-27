-module(istype_lib).

-include("istype.hrl").

-export([istype/2, istype/5, totype/2, totype/5]).

-define(conversion_error(Value, Type), ?conversion_error(Value, Type, undefined)).
-define(conversion_error(Value, Type, Reason), error({istype_conversion, Value, Type, Reason})).

%%==============================================================================
%% istype functions
%%==============================================================================
%% istype
%%==========================================================
istype(Value, Type) ->
    istype(Value, Type, #{}, #{}, []).

istype(Value, Type, Types, Records, Options) ->
    do_istype(Value, Type, Types, Records, Options).
%%==========================================================
%% do_istype
%%==========================================================
%% @doc Validates the given value is the given type.
%% @end
%%======================================
%% ShortHand
%%======================================
do_istype(Value, Type, Types, Records, Options) when is_atom(Type) ->
    shorthand(Value, Type, Types, Records, Options, fun do_istype/5);
%%======================================
%% Literals
%%======================================
do_istype(Value, #literal{value = Literal}, _, _, _) ->
    Value =:= Literal;
%%======================================
%% any()
%%======================================
do_istype(_, #type{type = any}, _, _, _) ->
    true;
%%======================================
%% none()
%%======================================
do_istype(_, #type{type = none}, _, _, _) ->
    false;
%%======================================
%% pid()
%%======================================
do_istype(Value, #type{type = pid}, _, _, _) ->
    is_pid(Value);
%%======================================
%% port()
%%======================================
do_istype(Value, #type{type = port}, _, _, _) ->
    is_port(Value);
%%======================================
%% reference()
%%======================================
do_istype(Value, #type{type = reference}, _, _, _) ->
    is_reference(Value);
%%======================================
%% [] - nil()
%%======================================
do_istype(Value, #type{type = nil}, _, _, _) ->
    Value =:= [];
%%======================================
%% Atom
%%======================================
%% atom()
%%==================
do_istype(Value, #type{type = atom}, _, _, _) ->
    is_atom(Value);
%%==================
%% Erlang_Atom
%%==================
%% @doc Handled by literal handler
%% @end
%%======================================
%% Bitstring
%%======================================
do_istype(Value, #type{type = bitstring, spec = Format}, _, _, _) ->
    is_valid_bitstring(Value, Format);
%%======================================
%% float()
%%======================================
do_istype(Value, #type{type = float}, _, _, _) ->
    is_float(Value);
%%======================================
%% Fun
%%======================================
do_istype(Value, #type{type = 'fun'}, _, _, _) ->
    is_function(Value);
%%======================================
%% Integer
%%======================================
%% integer()
%%==================
do_istype(Value, #type{type = integer}, _, _, _) ->
    is_integer(Value);
%%==================
%% Erlang_Integer
%%==================
%% @doc Handled by the default handler
%% @end
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
do_istype(Value, #type{type = range, spec = {LowerBound, UpperBound}}, _, _, _) when is_integer(Value) ->
    case Value of
        _ when LowerBound#literal.value =:= undefined andalso Value =< UpperBound#literal.value ->
            true;
        _ when UpperBound#literal.value =:= undefined andalso Value >= LowerBound#literal.value ->
            true;
        _ when Value >= LowerBound#literal.value andalso Value =< UpperBound#literal.value ->
            true;
        _ ->
            false
    end;
%%======================================
%% List
%%======================================
do_istype(Value, #type{type = list, spec = any}, _, _, _) ->
    is_list(Value);
do_istype(Value, #type{type = list, spec = TypeSpec}, Types, Records, Options) when is_list(Value) ->
    {Empty, ValueType, TerminatorType} = TypeSpec,

    io:format("Evaluating list ~p ~p\n", [TypeSpec, Value]),

    do_istype_list(Value, Empty, ValueType, TerminatorType, Types, Records, Options);
%%======================================
%% Map
%%======================================
do_istype(Value, #type{type = map, spec = any}, _, _, _) ->
    is_map(Value);
do_istype(Value, #type{type = map} = Type, Types, Records, Options) when is_map(Value) ->
    do_istype_map(Value, Type, Types, Records, Options);
%%======================================
%% Tuple
%%======================================
do_istype(Value, #type{type = tuple, spec = any}, _, _, _) ->
    is_tuple(Value);
do_istype(Value, #type{type = tuple, spec = empty}, _, _, _) ->
    Value =:= {};
do_istype(Value, #type{type = tuple} = Type, Types, Records, Options) when is_tuple(Value) ->
    do_istype_tuple(Value, Type, Types, Records, Options);
%%======================================
%% Union
%%======================================
do_istype(_, #type{type = union, spec = []}, _, _, _) ->
    false;
do_istype(Value, #type{type = union, spec = [UnionType | UnionTypes]} = Type, Types, Records, Options) ->
    io:format("Types ~p\n", [Types]),
    io:format("Check union type ~p ~p ~p\n", [UnionType, Value, do_istype(Value, UnionType, Types, Records, Options)]),
    case do_istype(Value, UnionType, Types, Records, Options) of
        true ->
            true;
        false ->
            do_istype(Value, Type#type{spec = UnionTypes}, Types, Records, Options)
    end;
%%======================================
%% term()
%%======================================
%% @doc Converted to any() during parse.
%% @end
%%======================================
%% binary()
%%======================================
do_istype(Value, #type{type = binary}, _, _, _) ->
    is_binary(Value);
%%======================================
%% bitstring()
%%======================================
%% @doc Handled by Bitstring.
%% @end
%%======================================
%% boolean()
%%======================================
do_istype(Value, #type{type = boolean}, _, _, _) ->
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
%% @doc Handle a custom type.
%% @end
do_istype(Value, Base, Types, Records, Options) ->
    MFA = {Base#type.module,
           Base#type.type,
           length(Base#type.params)},
    case Types of
        #{MFA := Type} ->
            io:format("Custom Type ~p ~p\n", [MFA, Type]),
            do_istype(Value, Type, Types, Records, Options);
        _ ->
            false
    end.

%%==========================================================
%% do_istype_list()
%%==========================================================
do_istype_list([], nonempty, _, _, _, _, _) ->
    io:format("Empty but must be nonempty\n", []),
    false;
do_istype_list([], maybe_empty, _, _, _, _, _) ->
    io:format("Empty but thats ok\n", []),
    true;
do_istype_list([Value | []], _, ValueType, TerminatorType, Types, Records, Options) ->
    io:format("Value then nil terminated ~p ~p\n", [do_istype(Value, ValueType, Types, Records, Options), do_istype([], TerminatorType, Types, Records, Options)]),
    io:format("~p ~p\n", [Value, ValueType]),
    io:format("~p ~p\n", [[], TerminatorType]),
    do_istype(Value, ValueType, Types, Records, Options) andalso
    do_istype([], TerminatorType, Types, Records, Options);
do_istype_list([Value | Values], Empty, ValueType, TerminatorType, Types, Records, Options) when is_list(Values) ->
    io:format("Value then continues ~p\n", [do_istype(Value, ValueType, Types, Records, Options)]),
    io:format("~p ~p\n", [Value, ValueType]),

    do_istype(Value, ValueType, Types, Records, Options) andalso
    do_istype_list(Values, Empty, ValueType, TerminatorType, Types, Records, Options);
do_istype_list([Value | Terminator], _, ValueType, TerminatorType, Types, Records, Options) ->
    io:format("Value then notnil terminated ~p ~p\n", [do_istype(Value, ValueType, Types, Records, Options), do_istype(Terminator, TerminatorType, Types, Records, Options)]),

    io:format("~p ~p\n", [Value, ValueType]),
    io:format("~p ~p\n", [Terminator, TerminatorType]),

    do_istype(Value, ValueType, Types, Records, Options) andalso
    do_istype(Terminator, TerminatorType, Types, Records, Options).

%%==========================================================
%% do_istype_map()
%%==========================================================
do_istype_map(Value, #type{type = map, spec = {Exact, _}}, Types, Records, Options) ->
    Results = maps:fold(fun(_, _, {[], true} = True) ->
                               True; 
                           (K0, V0, {ExactAcc0, true}) ->
                               io:format("Validate map assoc ~p\n", [ExactAcc0]),
                               case do_istype_map_assoc(K0, V0, ExactAcc0, Types, Records, Options) of
                                   {true, Assoc} ->
                                       {ExactAcc0 -- [Assoc], true};
                                   false ->
                                       {ExactAcc0, false}
                               end;
                           (_, _, False) ->
                               False
                        end,
                        {Exact, true},
                        Value),

    case Results of
        {[], true} ->
            true;
        _ ->
            false
    end.

do_istype_map_assoc(_, _, [], _, _, _) ->
    false;
do_istype_map_assoc(Key, Value, [AssocType | AssocTypes], Types, Records, Options) ->
    {KeyType, ValueType} = AssocType,
    Result = do_istype(Key, KeyType, Types, Records, Options) andalso
             do_istype(Value, ValueType, Types, Records, Options),
    case Result of
        true ->
            {true, AssocType};
        false ->
            do_istype_map_assoc(Key, Value, AssocTypes, Types, Records, Options)
    end.

%%==========================================================
%% do_istype_tuple()
%%==========================================================
do_istype_tuple(Value, #type{type = tuple, spec = TupleTypes}, Types, Records, Options) ->
    case length(TupleTypes) of
        X when X =:= size(Value) ->
            do_istype_tuple(Value, TupleTypes, Types, Records, Options, true, 1);
        _ ->
            false
    end.

do_istype_tuple(_, _, _, _, _, false, _) ->
    false;
do_istype_tuple(_, [], _, _, _, _, _) ->
    false;
do_istype_tuple(Value, [TupleType | TupleTypes], Types, Records, Options, _, Index) ->
    do_istype_tuple(Value,
                    TupleTypes,
                    Types,
                    Records,
                    Options,
                    do_istype(element(Index, Value), TupleType, Types, Records, Options),
                    Index + 1).

%%==============================================================================
%% totype functions
%%==============================================================================
%% totype()
%%==========================================================
totype(Value, Type) ->
    totype(Value, Type, #{}, #{}, []).

totype(Value, Type, Types, Records, Options) ->
    try
        io:format("Convert\n\tValues ~p\n\tType ~p\n", [Value, Type]),
        do_totype(Value, Type, Types, Records, Options)
    catch
        error:{conversion_error, V1, T1, Reason} ->
            ?conversion_error(Value, Type, {V1, T1, Reason});
        Class:Error ->
            Reason = {Class, Error, erlang:get_stacktrace()},
            ?conversion_error(Value, Type, Reason)
    end.

%%==========================================================
%% do_totype()
%%==========================================================
%% @doc Converts the given value to the given type.
%% @end
%%======================================
%% ShortHand
%%======================================
do_totype(Value, Type, Types, Records, Options) when is_atom(Type) ->
    shorthand(Value, Type, Types, Records, Options, fun do_totype/5);
%%======================================
%% any()
%%======================================
do_totype(Value, #type{type = any}, _, _, _) ->
    Value;
%%======================================
%% none()
%%======================================
do_totype(Value, #type{type = none} = Type, _, _, _) ->
    ?conversion_error(Value, Type, convert_to_none);
%%======================================
%% pid()
%%======================================
do_totype(Value, #type{type = pid}, _, _, _) when is_pid(Value) ->
    Value;
do_totype(Value, #type{type = pid}, _, _, _) when is_list(Value) ->
    list_to_pid(Value);
do_totype(Value, #type{type = pid}, Types, Records, Options) when is_binary(Value) ->
    AsList = do_totype(Value, list, Types, Records, Options),
    do_totype(AsList, pid, Types, Records, Options);
%%======================================
%% port()
%%======================================
do_totype(Value, #type{type = port}, _, _, _) when is_port(Value) ->
    Value;
do_totype(Value, #type{type = port}, _, _, _) when is_list(Value) ->
    list_to_port(Value);
do_totype(Value, #type{type = port}, Types, Records, Options) when is_binary(Value) ->
    AsList = do_totype(Value, list, Types, Records, Options),
    do_totype(AsList, port, Types, Records, Options);
%%======================================
%% reference()
%%======================================
do_totype(Value, #type{type = reference}, _, _, _) when is_reference(Value) ->
    Value;
do_totype(Value, #type{type = reference}, _, _, _) when is_list(Value) ->
    list_to_ref(Value);
do_totype(Value, #type{type = reference}, Types, Records, Options) when is_binary(Value) ->
    AsList = do_totype(Value, list, Types, Records, Options),
    do_totype(AsList, reference, Types, Records, Options);
%%======================================
%% [] - nil()
%%======================================
do_totype(<<>>, #literal{value = []}, _, _, _) ->
    [];
do_totype([], #literal{value = []}, _, _, _) ->
    [];    
%%======================================
%% Atom
%%======================================
%%==================
%% atom()
%%==================
do_totype(Value, #type{type = atom}, _, _, _) when is_atom(Value) ->
    Value;
do_totype(Value, #type{type = atom}, _, _, _) when is_binary(Value) ->
    binary_to_atom(Value, utf8);
do_totype(Value, #type{type = atom}, _, _, _) when is_list(Value) ->
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
do_totype(<<>>, #type{type = bitstring, spec = {0, 0}}, _, _, _) ->
    <<>>;
do_totype([], #type{type = bitstring, spec = {0, 0}}, _, _, _) ->
    <<>>;
do_totype(Value, #type{type = bitstring, spec = Format}, _, _, _) when is_bitstring(Value) ->
    case is_valid_bitstring(Value, Format) of
        true ->
            Value;
        _ ->
            ?conversion_error(Value, {type, bitstring, Format}, bitstring_length)
    end;
do_totype(Value, #type{type = bitstring} = Type, Types, Records, Options) when is_atom(Value) ->
    AsBinary = do_totype(Value, binary, Types, Records, Options),
    do_totype(AsBinary, Type, Types, Records, Options);
%% @doc Even though we support binary -> bitstring there is no need to do any work
%%      as binary is a subset of bitstring.
%% @end
%do_totype(Value, {type, bitstring, _} = Type, Types, Records) when is_binary(Value) ->
%    AsBinary = do_totype(Value, binary, Types, Records),
%    do_totype(AsBinary, Type, Types, Records);
do_totype(Value, #type{type = bitstring} = Type, Types, Records, Options) when is_list(Value) ->
    AsBinary = do_totype(Value, binary, Types, Records, Options),
    do_totype(AsBinary, Type, Types, Records, Options);
%%======================================
%% float()
%%======================================
do_totype(Value, #type{type = float}, _, _, _) when is_float(Value) ->
    Value;
do_totype(Value, #type{type = float}, _, _, _) when is_integer(Value) ->
    1.0 * Value;
do_totype(Value, #type{type = float}, Types, Records, Options) when is_binary(Value) orelse
                                                                  is_list(Value) ->
    AsNumber = do_totype(Value, number, Types, Records, Options),
    do_totype(AsNumber, float, Types, Records, Options);
%%======================================
%% Fun
%%======================================
do_totype(Value, #type{type = 'fun'} = Type, _, _, _) ->
    ?conversion_error(Value, Type, convert_to_fun);
%%======================================
%% Integer
%%======================================
%%==================
%% integer()
%%==================
do_totype(Value, #type{type = integer}, _, _, _) when is_float(Value) ->
    trunc(Value);
do_totype(Value, #type{type = integer}, _, _, _) when is_integer(Value) ->
    Value;
do_totype(Value, #type{type = integer}, Types, Records, Options) when is_binary(Value) orelse
                                                                    is_list(Value) ->
    AsNumber = do_totype(Value, number, Types, Records, Options),
    do_totype(AsNumber, integer, Types, Records, Options);
%%==================
%% Erlang_Integer
%%==================
%% @doc Handled by the literal handler
%% @end
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
do_totype(Value, #type{type = range, spec = {#literal{value = LowerBound}, #literal{value = UpperBound}}} = Type, Types, Records, Options) ->
    AsInteger = do_totype(Value, integer, Types, Records, Options),
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
            ?conversion_error(Value, Type, out_of_range)
    end;
%%======================================
%% List
%%======================================
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_list(Value) ->
    Value;
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_pid(Value) ->
    pid_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_port(Value) ->
    port_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_reference(Value) ->
    ref_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_atom(Value) ->
    atom_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_bitstring(Value) ->
    bitstring_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_binary(Value) ->
    binary_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_float(Value) ->
    float_to_list(Value, [{decimals, 20}, compact]);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_integer(Value) ->
    integer_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_map(Value) ->
    maps:to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_function(Value) ->
    erlang:fun_to_list(Value);
do_totype(Value, #type{type = list, spec = any}, _, _, _) when is_tuple(Value) andalso 0 =:= size(Value) ->
    [];
do_totype(Value, #type{type = list, spec = {Empty, _, _}} = Type, _, _, _) when is_tuple(Value) andalso 0 =:= size(Value) ->
    case Empty of
        maybe_empty ->
            [];
        nonempty ->
            ?conversion_error(Value, Type, empty_tuple)
    end;
do_totype(Value, #type{type = list, spec = any}, Types, Records, Options) when is_tuple(Value) ->
    case maps:get(element(1, Value), Records, undefined) of
        {Arity, Fields, _} when size(Value) =:= Arity ->
            AsRecord = do_totype(Value, {record, element(1, Value), []}, Types, Records, Options),
            lists:zip(Fields, tl(tuple_to_list(AsRecord)));
        _ ->
            tuple_to_list(Value)
    end;
do_totype(Value, #type{type = list, spec = {Empty, ValueType, TerminatorType}} = Type, Types, Records, Options) ->
    try
        AsList = do_totype(Value, list, Types, Records, Options),
        do_totype_list(AsList, Empty, ValueType, TerminatorType, Types, Records, Options)
    catch
        error:{istype_conversion, ErrorValue, ErrorType, ErrorReason} ->
            ?conversion_error(Value, Type, {ErrorValue, ErrorType, ErrorReason});
        Class:Error ->
            Reason = {Class, Error, erlang:get_stacktrace()},
            ?conversion_error(Value, Type, Reason)
    end;
%%======================================
%% Map
%%======================================
do_totype(#{}, #type{type = map, spec = empty}, _, _, _) ->
    #{};
do_totype([], #type{type = map, spec = empty}, _, _, _) ->
    #{};
do_totype(<<>>, #type{type = map, spec = empty}, _, _, _) ->
    #{};
do_totype(Value, #type{type = map, spec = any}, _, _, _) when is_map(Value) ->
    Value;
do_totype(Value, #type{type = map, spec = any} = Type, _, _, _) when is_list(Value) ->
    try
        maps:from_list(Value)
    catch
        error:{istype_conversion, ErrorValue, ErrorType, ErrorReason} ->
            ?conversion_error(Value, Type, {ErrorValue, ErrorType, ErrorReason});
        Class:Error ->
            Reason = {Class, Error, erlang:get_stacktrace()},
            ?conversion_error(Value, Type, Reason)
    end;
do_totype(Value, #type{type = map, spec = any} = Type, Types, Records, Options) when is_tuple(Value) ->
    case maps:get(element(1, Value), Records, undefined) of
        #record{arity = Arity, fields = Fields} when size(Value) =:= Arity ->
            do_totype(lists:zip(Fields, tl(tuple_to_list(Value))), map, Types, Records, Options);
        undefined ->
            ?conversion_error(Value, Type, tupel_to_map_no_record)
    end;
do_totype(Value, #type{type = map, spec = any} = Type, _, _, _) ->
    ?conversion_error(Value, Type, no_map_conversion);
do_totype(Value, #type{type = map} = Type, Types, Records, Options) ->
    AsMap = do_totype(Value, Type#type{spec = any}, Types, Records, Options),
    do_totype_map(AsMap, Type, Types, Records, Options);
%%======================================
%% Tuple
%%======================================
do_totype(Value, #type{type = tuple, spec = any}, _, _, _) when is_tuple(Value) ->
    Value;
do_totype(Value, #type{type = tuple, spec = any}, _, _, _) when is_list(Value) ->
    list_to_tuple(Value);
do_totype({}, #type{type = tuple, spec = empty}, _, _, _) ->
    {};
do_totype(<<>>, #type{type = tuple, spec = empty}, _, _, _) ->
    {};
do_totype([], #type{type = tuple, spec = empty}, _, _, _) ->
    {};
do_totype(Value, #type{type = tuple} = Type, Types, Records, Options) when is_tuple(Value) ->
    do_totype(tuple_to_list(Value), Type, Types, Records, Options);
do_totype(Value, #type{type = tuple, spec = Spec} = Type, Types, Records, Options) when is_list(Value) ->
    {TupleSize, FieldTypes} = Spec,
    ValueSize = length(Value),

    case TupleSize of
        ValueSize ->
            ok;
        _ ->
            ?conversion_error(Value, Type, list_to_tuple_bad_length)
    end,

    Converted = lists:map(fun({FieldValue, {_, FieldType}}) ->
                              do_totype(FieldValue, FieldType, Types, Records, Options)
                          end,
                          lists:zip(Value, FieldTypes)),
    list_to_tuple(Converted);
%%======================================
%% Union
%%======================================
do_totype(Value, #type{type = union, spec = []} = Type, _, _, _) ->
    ?conversion_error(Value, Type, no_union_type);
do_totype(Value, #type{type = union, spec = [UnionType | UnionTypes]} = Type, Types, Records, Options) ->
    try
        io:format("Try Convert ~p -> ~p\n", [Value, UnionType]),
        do_totype(Value, UnionType, Types, Records, Options)
    catch
        _:_ ->
            do_totype(Value, Type#type{type = union, spec = UnionTypes}, Types, Records, Options)
    end;
%%======================================
%% term()
%%======================================
%% @doc Converted to any() during parse
%% @end
%%======================================
%% binary()
%%======================================
do_totype(Value, #type{type = binary}, _, _, _) when is_binary(Value) ->
    Value;
do_totype(Value, #type{type = binary}, Types, Records, Options) when is_pid(Value) orelse
                                                                   is_port(Value) orelse
                                                                   is_reference(Value) orelse
                                                                   is_function(Value) ->
    list_to_binary(do_totype(Value, list, Types, Records, Options));
do_totype(Value, #type{type = binary}, _, _, _) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
do_totype(Value, #type{type = binary}, _, _, _) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 20}, compact]);
do_totype(Value, #type{type = binary}, _, _, _) when is_integer(Value) ->
    integer_to_binary(Value);
do_totype(Value, #type{type = binary}, _, _, _) when is_list(Value) ->
    list_to_binary(Value);
%%======================================
%% bitstring()
%%======================================
do_totype(Value, #type{type = bitstring}, _, _, _) when is_bitstring(Value) orelse
                                                      is_binary(Value) ->
    Value;
do_totype(Value, #type{type = bitstring}, Types, Records, Options) when is_pid(Value) orelse
                                                                      is_port(Value) orelse
                                                                      is_reference(Value) orelse
                                                                      is_atom(Value) orelse
                                                                      is_float(Value) orelse
                                                                      is_integer(Value) orelse
                                                                      is_function(Value) ->
    AsList = do_totype(Value, list, Types, Records, Options),
    do_totype(AsList, bitstring, Types, Records, Options);
do_totype(Value, #type{type = bitstring}, _, _, _) when is_list(Value) ->
    list_to_bitstring(Value);
%%======================================
%% boolean()
%%======================================
do_totype(Value, #type{type = boolean} = Type, Types, Records, Options) ->
    AsAtom = do_totype(Value, atom, Types, Records, Options),
    case AsAtom of
        true ->
            true;
        false ->
            false;
        _ ->
            ?conversion_error(Value, Type, bad_boolean)
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
do_totype(Value, #type{type = number}, _, _, _) when is_float(Value) orelse
                                                   is_integer(Value) ->
    Value;
do_totype(Value, #type{type = number}, Types, Records, Options) when is_list(Value) ->
    AsBinary = do_totype(Value, binary, Types, Records, Options),
    do_totype(AsBinary, number, Types, Records, Options);
do_totype(Value, #type{type = number}, _, _, _) when is_binary(Value) ->
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
do_totype(Value, {record_spec, _, _, _} = Record, Types, Records, Options) when is_list(Value) ->
    do_totype(maps:from_list(Value), Record, Types, Records, Options);
do_totype(Value, {record_spec, Record, {_, _, _, FieldTypes}, Module}, Types, Records, Options) when is_map(Value) ->
    Default = Module:istype_default_records(Record),
    AtomMap = maps:fold(fun(K0, V, Acc) ->
                            K1 = do_totype(K0, atom, Types, Records, Options),
                            Acc#{K1 => V}
                        end,
                        #{},
                        Value),
    {_, Result} = lists:foldl(fun({F, T}, {Index, Acc}) ->
                                  case maps:get(F, AtomMap, undefined) of
                                      undefined ->
                                          {Index + 1, Acc};
                                      FieldValue ->
                                          io:format("Converting ~p ~p\n", [FieldValue, T]),
                                          {Index + 1, setelement(Index, Acc, do_totype(FieldValue, T, Types, Records, Options))}
                                  end
                              end,
                              {2, Default},
                              FieldTypes),
    Result;
%%======================================
%% Literals
%%======================================
%% @doc Support for conversion to literals is limited to the types that
%%      are allowed within a typespec, an empty list [], or an empty map #{}.
%% @end
do_totype(Value, #literal{value = LiteralValue} = Literal, Types, Records, Options) ->
    io:format("Convert Literal ~p -> ~p\n", [Value, LiteralValue]),
    case LiteralValue of
        _ when is_atom(LiteralValue) ->
            LiteralValue = do_totype(Value, atom, Types, Records, Options);
        _ when is_integer(LiteralValue) -> 
            LiteralValue = do_totype(Value, integer, Types, Records, Options);
        [] ->
            LiteralValue = do_totype(Value, list, Types, Records, Options);
        #{} ->
            LiteralValue = do_totype(Value, map, Types, Records, Options);
        _ -> 
            ?conversion_error(Value, Literal, convert_to_literal)
    end;
%%======================================
%% Custom Type
%%======================================
do_totype(Value, {type, TypeName, []}, Types, Records, Options) ->
    #{TypeName := Type} = Types,
    do_totype(Value, Type, Types, Records, Options);
%%======================================
%% No conversion
%%======================================
do_totype(Value, Type, _, _, _) ->
    ?conversion_error(Value, Type, no_conversion).

%%==========================================================
%% do_totype_list
%%==========================================================
%% @doc Convert into the specific list typing.
%% @end
do_totype_list(Value, Empty, ValueType, TerminatorType, Types, Records, Options) ->
    do_totype_list(Value, Empty, ValueType, TerminatorType, Types, Records, Options, []).
%% @doc Nonempty with an empty input is invalid.
%% @end
do_totype_list([], nonempty, ValueType, TerminatorType, _, _, _, _) ->
    ?conversion_error([], #type{type = list, spec = {nonempty, ValueType, TerminatorType}});
%% @doc Maybe empty with an empty input is ok if [] is a valid terminator type.
%% @end
do_totype_list([], maybe_empty, ValueType, TerminatorType, Types, Records, Options, _) ->
    case do_istype([], TerminatorType, Types, Records, Options) of
        true ->
            [];
        false ->
            ?conversion_error([], #type{type = list, spec = {maybe_empty, ValueType, TerminatorType}})
    end;
%% @doc When we see a nil tail, validate that nil is a valid tail,
%%      add to the Acc, and reverse the result.
%% @end
do_totype_list([Hd | []], _, ValueType, TerminatorType, Types, Records, Options, Acc) ->
    case istype([], TerminatorType, Types, Records, Options) of
        true ->
            lists:reverse([do_totype(Hd, ValueType, Types, Records, Options) | Acc]);
        false ->
            ?conversion_error([], TerminatorType)
    end;
%% @doc When tail is a list convert the value and recurse.
%% @end
do_totype_list([Hd | Tl], Empty, ValueType, TerminatorType, Types, Records, Options, Acc0) when is_list(Tl) ->
    Acc1 = [do_totype(Hd, ValueType, Types, Records, Options) | Acc0],
    do_totype_list(Tl, Empty, ValueType, TerminatorType, Types, Records, Options, Acc1);
%% @doc When tail is not a list then it's the terminator of an improper list.
%%      Convert the value and terminator then generate an improper list.
%% @end
do_totype_list([Hd0 | Tl0], _, ValueType, TerminatorType, Types, Records, Options, Acc) ->
    Hd1 = lists:reverse([do_totype(Hd0, ValueType, Types, Records, Options) | Acc]),
    Tl1 = do_totype(Tl0, TerminatorType, Types, Records, Options),
    make_improper_list(Hd1, Tl1).

make_improper_list([], Terminator) ->
    Terminator;
make_improper_list([Hd | Tl], Terminator) ->
    [Hd | make_improper_list(Tl, Terminator)].

%%==========================================================
%% do_totype_map
%%==========================================================
do_totype_map(Value0, #type{spec = {Exact, Assoc}} = Type, Types, Records, Options) ->
    %% Maps typing can be ambiguous on conversion. Consider
    %% #{<<"apple">> => "apple"} against the type spec #{string() := string(), binary() := binary()}.
    %% On conversion both associations are eligible for conversion but with no clear definition on which 
    %% should have priority. To make this behavior predictable conversion will only be performed
    %% on associations that have a literal for the key or index.
    
    KVs0 = lists:foldr(fun({#literal{}, _} = KV, Acc) ->
                              [KV | Acc];
                          ({_, #literal{}} = KV, Acc) ->
                              [KV | Acc];
                          (_, Acc) ->
                              Acc
                       end,
                       [],
                       Exact),

    KVs1 = lists:foldr(fun({#literal{}, _} = KV, Acc) ->
                              [KV | Acc];
                          ({_, #literal{}} = KV, Acc) ->
                              [KV | Acc];
                          (_, Acc) ->
                              Acc
                       end,
                       KVs0,
                       Assoc),

    %% Generate a new map of converted types while reducing the set of required fields.
    Value1 = maps:fold(fun(K0, V0, ValueAcc) ->
                           case do_istype_map_assoc(K0, V0, KVs1, Types, Records, Options) of
                               {true, _} ->
                                   ValueAcc#{K0 => V0};
                               false ->
                                   {K1, V1, _} = do_totype_map_assoc(K0, V0, KVs1, Types, Records, Options),
                                   ValueAcc#{K1 => V1}
                           end
                       end,
                       #{},
                       Value0),

    %% See if our limited edits were enough to make a valid map.
    case do_istype(Value1, Type, Types, Records, Options) of
        true ->
            Value1;
        false ->
            ?conversion_error(Value0, Type, {map_unable_to_convert})
    end.

do_totype_map_assoc(Key, Value, [], _, _, _) ->
    {Key, Value, undefined};
do_totype_map_assoc(Key, Value, [AssocType | AssocTypes], Types, Records, Options) ->
    try
        io:format("Try Convert {~p, ~p} => ~p\n", [Key, Value, AssocType]),
        {KeyType, ValueType} = AssocType,
        ConvertedKey = do_totype(Key, KeyType, Types, Records, Options),
        ConvertedValue = do_totype(Value, ValueType, Types, Records, Options),
        {ConvertedKey, ConvertedValue, AssocType}
    catch
        _:_ ->
            do_totype_map_assoc(Key, Value, AssocTypes, Types, Records, Options)
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
shorthand(Value, list, Types, Records, Options, Action) ->
    Action(Value, #type{type = list, spec = any}, Types, Records, Options);
shorthand(Value, map, Types, Records, Options, Action) ->
    Action(Value, #type{type = map, spec = any}, Types, Records, Options);
shorthand(Value, Type, Types, Records, Options, Action) ->
    Action(Value, #type{type = Type}, Types, Records, Options).
