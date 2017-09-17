-module(istype_lib).
-export([convert/4, validate/4]).

%% @doc Master conversion function.
%%      Attempts to convert X into a type as specified by typedata.
%%      In the event that X is a tuple/record all typed fields are
%%      recursively converted.
%%      In the event of a union the first successful conversion is returned.
%% @end
convert(Value, Type, Types, Records) ->
    try
        do_convert(Value, Type, Types, Records)
    catch
        error:Error when element(1, Error) =:= istype_conversion ->
            error(Error);
        _:_ ->
            convert_error(Value, Type)
    end.

do_convert(X, {literal, atom, Y}, Types, Records) ->
    Y = do_convert(X, {type, atom, []}, Types, Records);
do_convert(X, {literal, integer, Y}, Types, Records)  ->
    Y = do_convert(X, {type, integer, []}, Types, Records);

do_convert(X, {type, any, _}, _, _) ->
    X;
%% @doc Convert to atom
%% @end
do_convert(X, {type, atom, _}, _, _) when is_atom(X) ->
    X;
do_convert(X, {type, atom, _}, _, _) when is_binary(X) ->
    %% TODO: Figure out how to make existing and encoding configurable
    binary_to_existing_atom(X, utf8);
do_convert(X, {type, atom, _}, _, _) when is_list(X) ->
    list_to_existing_atom(X);
%% @doc convert to binary
%% @end
do_convert(X, {type, binary, _}, _, _) when is_atom(X) ->
    %% TODO: Figure out how to make encoding configurable
    atom_to_binary(X, utf8);
do_convert(X, {type, binary, _}, _, _) when is_binary(X) ->
    X;
do_convert(X, {type, binary, _}, _, _) when is_float(X) ->
    %% TODO: Figure out how to make option configurable
    float_to_binary(X);
do_convert(X, {type, binary, _}, _, _) when is_integer(X) ->
    integer_to_binary(X);
do_convert(X, {type, binary, _}, _, _) when is_list(X) ->
    list_to_binary(X);
do_convert(X, {type, binary, _}, Types, Records) when is_function(X) orelse
                                                      is_pid(X) orelse
                                                      is_port(X) orelse
                                                      is_reference(X) ->
    do_convert(do_convert(X, {type, list, []}, Types, Records), {type, binary, []}, Types, Records);
%% @doc convert to bitstring
%% @end
do_convert(X, {type, bitstring, _}, Types, Records) when is_atom(X) orelse
                                                         is_float(X) orelse
                                                         is_integer(X) orelse
                                                         is_function(X) orelse
                                                         is_pid(X) orelse
                                                         is_port(X) orelse
                                                         is_reference(X) ->
    do_convert(do_convert(X, {type, binary, []}, Types, Records),
               {type, bitstring, []},
               Types,
               Records);
do_convert(X, {type, bitstring, _}, _, _) when is_binary(X) orelse
                                               is_bitstring(X) ->
    X;
do_convert(X, {type, bitstring, _}, _, _) when is_list(X) ->
    list_to_bitstring(X);
%% @doc convert to boolean
%% @end
do_convert(X, {type, boolean, _}, _, _) when is_atom(X) andalso
                                       (X =:= true orelse
                                        X =:= false) ->
    X;
do_convert(X, {type, boolean, _}, Types, Records) when is_binary(X) ->
    do_convert(do_convert(X, {type, atom, []}, Types, Records), {type, boolean, []}, Types, Records);
do_convert(X, {type, boolean, _}, Types, Records) when is_list(X) ->
    do_convert(do_convert(X, {type, atom, []}, Types, Records), {type, boolean, []}, Types, Records);
%% @doc float
do_convert(X, {type, float, _}, Types, Records) when is_binary(X) ->
    do_convert(do_convert(X, {type, number, []}, Types, Records), {type, float, []}, Types, Records);
do_convert(X, {type, float, _}, _, _) when is_float(X) ->
    X;
do_convert(X, {type, float, _}, _, _) when is_integer(X) ->
    X * 1.0;
do_convert(X, {type, float, _}, Types, Records) when is_list(X) ->
    do_convert(do_convert(X, {type, number, []}, Types, Records), {type, float, []}, Types, Records);
%% @doc convert to integer
%% @end
do_convert(X, {type, integer, _}, Types, Records) when is_binary(X) ->
    %% TODO: Figure out how to make base configurable
    do_convert(do_convert(X, {type, number, []}, Types, Records), {type, integer, []}, Types, Records);
do_convert(X0, {type, integer, _}, _, _) when is_float(X0) ->
    X1 = trunc(X0),
    case X0 of
        _ when X0 == X1 ->
            X1;
        _ ->
            convert_error(X0, integer)
    end;
do_convert(X, {type, integer, _}, _, _) when is_integer(X) ->
    X;
do_convert(X, {type, integer, _}, Types, Records) when is_list(X) ->
    %% TODO: Figure out how to make base configurable
    do_convert(do_convert(X, {type, number, []}, Types, Records), {type, integer, []}, Types, Records);
%% @doc convert to list
%% @end
do_convert(X, {type, list, []}, _, _) when is_atom(X) ->
    atom_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_binary(X) ->
    binary_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_bitstring(X) ->
    bitstring_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_float(X) ->
    float_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_function(X) ->
    erlang:fun_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_integer(X) ->
    integer_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_list(X) ->
    X;
do_convert(X, {type, list, []}, _, _) when is_map(X) ->
    maps:to_list(X);
do_convert(X, {type, list, []}, _, _) when is_pid(X) ->
    pid_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_port(X) ->
    port_to_list(X);
do_convert(X, {type, list, []}, _, _) when is_reference(X) ->
    ref_to_list(X);
do_convert({}, {type, list, []}, _, _) ->
    [];
do_convert(X, {type, list, []}, Types, Records) when is_tuple(X) ->
    case get_record_spec(element(1, X), Types, Records) of
        undefined ->
            tuple_to_list(X);
        {Arity, Fields, _} when size(X) =:= Arity ->
            do_convert(X, {type, list_record, Fields}, Types, Records)
    end;
do_convert(X, {type, list_record, Fields}, Types, Records) ->
    record_to_list(list_record, X, Fields, Types, Records);
do_convert(X, {type, list, [{literal, atom, nonempty}, ListType, TerminatorType] = ListSpec}, Types, Records) ->
    List = do_convert(X, {type, list, [{literal, atom, empty}, ListType, TerminatorType]}, Types, Records),
    case List of
        [] ->
            convert_error(X, {type, list, ListSpec});
        _ ->
            List
    end;

%% Convert to a potentially empty proper list
do_convert(X, {type, list, [{literal, atom, empty}, ListType, {type, nil, []}]}, Types, Records) when is_list(X) ->
    convert_list_values(X, ListType, {type, nil, []}, Types, Records);
do_convert(X, {type, list, [{literal, atom, empty}, ListType, {type, nil, []}]}, Types, Records) ->
    ListX = do_convert(X, {type, list, []}, Types, Records),
    convert_list_values(ListX, ListType, {type, nil, []}, Types, Records);

%% @doc convert to map
%% @end
do_convert(X, {type, map, _}, _, _) when is_list(X) ->
    maps:from_list(X);
do_convert(X, {type, map, _}, _, _) when is_map(X) ->
    X;
do_convert(X, {type, map}, Types, Records) when is_tuple(X) ->
    case get_record_spec(element(1, X), Types, Records) of
        {Arity, Fields, _} when size(X) =:= Arity ->
            %% This value is a record handle it a bit differently
            do_convert(X, {type, map_record, Fields}, Types, Records)
    end;
do_convert(X, {type, map_record, Fields}, Types, Records) ->
    maps:from_list(record_to_list(map_record, X, Fields, Types, Records));

do_convert(X, {type, nil, NilArgs}, Types, Records) ->
    case do_convert(X, {type, list, []}, Types, Records) of
        Nil when length(Nil) =:= 0 -> Nil;
        _ -> convert_error(X, {type, nil, NilArgs})
    end;

%% @doc convert to number
%% @end
do_convert(X, {type, number, _}, _, _) when is_binary(X) ->
    try
        binary_to_integer(X)
    catch
        _:_ ->
            binary_to_float(X)
    end;
do_convert(X, {type, number, _}, _, _) when is_number(X) ->
    X;
do_convert(X, {type, number, _}, _, _) when is_list(X) ->
    try
        list_to_integer(X)
    catch
        _:_ ->
            list_to_float(X)
    end;
%% @doc convert to pid
%% @end
do_convert(X, {type, pid, _}, Types, Records) when is_binary(X) ->
    do_convert(do_convert(X, {type, list, []}, Types, Records), {type, pid, []}, Types, Records);
do_convert(X, {type, pid, _}, _, _) when is_list(X) ->
    list_to_pid(X);
do_convert(X, {type, pid, _}, _, _) when is_pid(X) ->
    X;
%% @doc convert to port
%% @end
do_convert(X, {type, port}, Types, Records) when is_binary(X) ->
    do_convert(do_convert(X, {type, list}, Types, Records), {type, port}, Types, Records);
do_convert(X, {type, port}, _, _) when is_list(X) ->
    list_to_port(X);
do_convert(X, {type, port}, _, _) when is_port(X) ->
    X;
%% @doc convert to range value
%% @end
do_convert(X0, {type, range, {Low, High}}, Types, Records) ->
    X1 = do_convert(X0, {type, integer, []}, Types, Records),
    case X1 of
        _ when Low =:= undefined andalso X1 > High ->
            convert_error(X0, {range, {Low, High}});
        _ when High =:= undefined andalso X1 < Low ->
            convert_error(X0, {range, {Low, High}});
        _ when Low =/= undefined andalso
               High =/= undefined andalso
               (X1 > High orelse X1 < Low) ->
            convert_error(X0, {range, {Low, High}});
        _ ->
            X1
    end;
%% @doc convert to reference
%% @end
do_convert(X, {type, reference, _}, Types, Records) when is_binary(X) ->
    do_convert(do_convert(X, {type, list, []}, Types, Records), {type, reference, []}, Types, Records);
do_convert(X, {type, reference, _}, _, _) when is_list(X) ->
    list_to_ref(X);
do_convert(X, {type, reference, _}, _, _) when is_reference(X) ->
    X;
%% @doc convert to union
%%      This is where conversion starts to get more complicated. Try
%%      each type in order until we successfully convert.
%% @end
do_convert(X, {type, union, UnionTypes}, Types, Records) ->
    do_convert(X, {type, union, UnionTypes, UnionTypes}, Types, Records);
do_convert(X, {type, union, UnionTypes, []}, _, _) ->
    convert_error(X, {union, UnionTypes});
do_convert(X, {type, union, UnionTypes, [Hd | Tl]}, Types, Records) ->
    try
        do_convert(X, Hd, Types, Records)
    catch
        _:_ ->
            do_convert(X, {type, union, UnionTypes, Tl}, Types, Records)
    end;
%% @doc convert to tuple
%% @end
do_convert(X, {type, tuple, any}, _, _) when is_list(X) ->
    list_to_tuple(X);
do_convert(X, {type, tuple, any}, _, _) when is_tuple(X) ->
    X;
do_convert({}, {type, tuple, none}, _, _) ->
    {};
do_convert([], {type, tuple, none}, _, _) ->
    {};
do_convert(X, {type, tuple, none}, _, _) ->
    convert_error(X, {type, tuple, none});
do_convert(X, {type, tuple, TupleTypes}, Types, Records) when is_list(X) ->
    do_convert(list_to_tuple(X), {type, tuple, TupleTypes}, Types, Records);
do_convert(X, {type, tuple, TupleTypes}, Types, Records) when is_tuple(X) ->
    lists:foldl(fun({Index, TupleType}, Acc) ->
                    setelement(Index, Acc, do_convert(element(Index, Acc), TupleType, Types, Records))
                end,
                X,
                lists:zip(lists:seq(1, length(TupleTypes)), TupleTypes));
%% @doc convert to a record
%% @end
do_convert(X, {type, record, _} = TypeSpec, Types, Records) when is_list(X) ->
    do_convert(maps:from_list(X), TypeSpec, Types, Records);
do_convert(X0, {type, record, {_, Default, _, ArityMap, FieldTuple}}, Types, Records) when is_map(X0) ->
    %% Standardize map indexes and remove entries that are not in the record
    X1 = maps:fold(fun(K0, V, Acc) ->
                       K1 = do_convert(K0, {type, atom, []}, Types, Records),
                       case maps:is_key(K1, ArityMap) of
                           true ->
                               Acc#{K1 => V};
                           false ->
                               Acc
                        end
                   end,
                   #{},
                   X0),
    maps:fold(fun(K, V, Acc) ->
                  #{K := Arity} = ArityMap,
                  setelement(Arity, Acc, do_convert(V, element(Arity, FieldTuple), Types, Records))
              end,
              Default,
              X1);

do_convert(X, {type, record, {Record, _, Arity, _, _} = RecordSpec}, Types, Records) when is_tuple(X) andalso
                                                                                      element(1, X) =:= Record andalso
                                                                                      Arity =:= size(X) ->
    {Record, Default, _, _, FieldTuple} = RecordSpec,
    lists:foldl(fun({Index, Type}, Acc) ->
                    setelement(Index, Acc, do_convert(element(Index, X), Type, Types, Records))
                end,
                Default,
                lists:zip(lists:seq(2, Arity), tl(tuple_to_list(FieldTuple))));
do_convert(X, {type, record, RecordSpec0}, Types, Records) ->
    Record = element(1, RecordSpec0),
    #{Record := RecordSpec1} = Records,
    do_convert(X, RecordSpec1, Types, Records);
do_convert(X, {type, Type, _} = Y, Types, Records) ->
    case maps:get(Type, Types, undefined) of
        undefined ->
            convert_error(X, Y);
        TypeSpec ->
            do_convert(X, TypeSpec, Types, Records)
    end.

convert_list_values(Values, ValueType, TerminatorType, Types, Records) ->
    convert_list_values(Values, [], ValueType, TerminatorType, Types, Records).

convert_list_values([], Acc, _, _, _, _) ->
    lists:reverse(Acc);
convert_list_values([Value | Terminator], Acc0, ValueType, {type, nil, _}, Types, Records) when not is_list(Terminator) ->
    Acc1 = [convert(Terminator, ValueType, Types, Records),
            convert(Value, ValueType, Types, Records) | Acc0],
    convert_list_values([], Acc1, ValueType, {type, nil, []}, Types, Records);
convert_list_values([Value | Values], Acc, ValueType, TerminatorType, Types, Records) when is_list(Values) ->
    convert_list_values(Values, [convert(Value, ValueType, Types, Records) | Acc], ValueType, TerminatorType, Types, Records).


%% @doc Format a conversion error
%% @end
convert_error(Value, Type) ->
    error({istype_conversion, Value, Type}).

get_record_spec(Record, Types, Records) ->
    try
        #{records := #{Record := RecordSpec}} = Types, Records,
        RecordSpec
    catch
        error:{badmatch, _} ->
            undefined
    end.

record_to_list(Nesting, X, Fields, Types, Records) ->
    Values = lists:map(fun(X0) when is_tuple(X0) ->
                              case get_record_spec(element(1, X0), Types, Records) of
                                  {Arity, Fields0, _} when size(X0) =:= Arity ->
                                      do_convert(X0, {type, Nesting, Fields0}, Types, Records);
                                  _ ->
                                      X0
                              end;
                          (X0) ->
                              X0
                       end,
                       tl(tuple_to_list(X))),
    lists:zip(Fields, Values).

validate(_, {type, any, []}, _, _) ->
    true;
validate(Value, {type, atom, []}, _, _) when is_atom(Value) ->
    true;
validate(Value, {type, binary, []}, _, _) when is_binary(Value) ->
    true;
validate(Value, {type, bitstring, []}, _, _) when is_bitstring(Value) ->
    true;
validate(Value, {type, boolean, []}, _, _) when is_boolean(Value) ->
    true;
validate(Value, {type, float, []}, _, _) when is_float(Value) ->
    true;
validate(Value, {type, function, []}, _, _) when is_function(Value) ->
    true;
validate(Value, {type, integer, []}, _, _) when is_integer(Value) ->
    true;
validate(Value, {type, list, []}, _, _) when is_list(Value) ->
    true;
validate(Value, {type, map, []}, _, _) when is_map(Value) ->
    true;
validate([], {type, nil, []}, _, _) ->
    true;
validate(Value, {type, number, []}, _, _) when is_number(Value) ->
    true;
validate(Value, {type, pid, []}, _, _) when is_pid(Value) ->
    true;
validate(Value, {type, port, []}, _, _) when is_port(Value) ->
    true;
validate(Value, {type, reference, []}, _, _) when is_reference(Value) ->
    true;
validate(Value, {type, union, UnionTypes}, Types, Records) ->
    validate_union_type(Value, UnionTypes, Types, Records);
validate(Value, {type, range, RangeSpec}, _, _) when is_integer(Value) ->
    case RangeSpec of
        {undefined, High} when Value < High ->
            true;
        {Low, undefined} when Value > Low ->
            true;
        {Low, High} when Value > Low andalso
                         Value < High ->
            true;
        _ ->
            false
    end;
validate(_, {type, range, _}, _, _) ->
    false;
validate(Value, {type, list, [{literal, atom, empty} | _]}, _, _) when Value =:= [] ->
    true;
validate(Value, {type, list, [{literal, atom, empty} | ListSpec]}, Types, Records) when is_list(Value) ->
    validate_list(Value, ListSpec, Types, Records);
validate(Value, {type, list, [{literal, atom, nonempty} | ListSpec]}, Types, Records) when is_list(Value) andalso
                                                                                           length(Value) > 0 ->
    validate_list(Value, ListSpec, Types, Records);
validate(Value, {type, Type0, _}, Types, Records)  ->
    case maps:get(Type0, Types, undefined) of
        undefined ->
            false;
        Type1 ->
            validate(Value, Type1, Types, Records)
    end.

validate_list(Value, [ValueType, TerminatorType], Types, Records) ->
    validate_list(Value, ValueType, TerminatorType, Types, Records).

validate_list([], _, {type, nil, []}, _, _) ->
    true;
validate_list([Value | Values], ValueType, TerminatorType, Types, Records) ->
    Check = {validate(Value, ValueType, Types, Records),
             validate(Values, TerminatorType, Types, Records)},

    case Check of
        {true, true} ->
            true;
        {true, false} when is_list(Values) ->
            validate_list(Values, ValueType, TerminatorType, Types, Records);
        _ ->
            false
    end.

validate_union_type(Value, UnionTypes, Types, Records) ->
    validate_union_type(Value, UnionTypes, Types, Records, false).

validate_union_type(_, [], _, _, Result) ->
    Result;
validate_union_type(_, _, _, _, true) ->
    true;
validate_union_type(Value, [UnionType | UnionTypes], Types, Records, _) ->
    validate_union_type(Value, UnionTypes, Types, Records, validate(Value, UnionType, Types, Records)).
