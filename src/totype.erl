-module(totype).
-export([convert/3]).

%% @doc Master conversion function.
%%      Attempts to convert X into a type as specified by typedata.
%%      In the event that X is a tuple/record all typed fields are
%%      recursively converted.
%%      In the event of a union the first successful conversion is returned.
%% @end
convert(Value, Type, UserTypes) ->
    try
        do_convert(Value, Type, UserTypes)
    catch
        error:Error when element(1, Error) =:= totype_conversion ->
            error(Error);
        E:O ->
            io:format("Found ~p:~p\n~p\n", [E, O, erlang:get_stacktrace()]),
            convert_error(Value, Type)
    end.

do_convert(X, {literal, Y}, UserTypes) when is_atom(Y) ->
    Y = do_convert(X, {type, atom}, UserTypes);
do_convert(X, {literal, Y}, UserTypes) when is_integer(Y) ->
    Y = do_convert(X, {type, integer}, UserTypes);

%% @doc Convert to atom
%% @end
do_convert(X, {type, atom}, _) when is_atom(X) ->
    X;
do_convert(X, {type, atom}, _) when is_binary(X) ->
    %% TODO: Figure out how to make existing and encoding configurable
    binary_to_existing_atom(X, utf8);
do_convert(X, {type, atom}, _) when is_list(X) ->
    list_to_existing_atom(X);
%% @doc convert to binary
%% @end
do_convert(X, {type, binary}, _) when is_atom(X) ->
    %% TODO: Figure out how to make encoding configurable
    atom_to_binary(X, utf8);
do_convert(X, {type, binary}, _) when is_binary(X) ->
    X;
do_convert(X, {type, binary}, _) when is_float(X) ->
    %% TODO: Figure out how to make option configurable
    float_to_binary(X);
do_convert(X, {type, binary}, _) when is_integer(X) ->
    integer_to_binary(X);
do_convert(X, {type, binary}, _) when is_list(X) ->
    list_to_binary(X);
do_convert(X, {type, binary}, UserTypes) when is_function(X) orelse
                                   is_pid(X) orelse
                                   is_port(X) orelse
                                   is_reference(X) ->
    do_convert(do_convert(X, {type, list}, UserTypes), {type, binary}, UserTypes);
%% @doc convert to bitstring
%% @end
do_convert(X, {type, bitstring}, _) when is_bitstring(X) ->
    X;
do_convert(X, {type, bitstring}, _) when is_list(X) ->
    list_to_bitstring(X);
%% @doc convert to boolean
%% @end
do_convert(X, {type, boolean}, _) when is_atom(X) andalso
                                       (X =:= true orelse
                                        X =:= false) ->
    X;
do_convert(X, {type, boolean}, UserTypes) when is_binary(X) ->
    do_convert(do_convert(X, {type, atom}, UserTypes), {type, boolean}, UserTypes);
do_convert(X, {type, boolean}, UserTypes) when is_list(X) ->
    do_convert(do_convert(X, {type, atom}, UserTypes), {type, boolean}, UserTypes);
%% @doc float
do_convert(X, {type, float}, UserTypes) when is_binary(X) ->
    do_convert(do_convert(X, {type, number}, UserTypes), {type, float}, UserTypes);
do_convert(X, {type, float}, _) when is_float(X) ->
    X;
do_convert(X, {type, float}, _) when is_integer(X) ->
    X * 1.0;
do_convert(X, {type, float}, UserTypes) when is_list(X) ->
    do_convert(do_convert(X, {type, number}, UserTypes), {type, float}, UserTypes);
%% @doc convert to integer
%% @end
do_convert(X, {type, integer}, UserTypes) when is_binary(X) ->
    %% TODO: Figure out how to make base configurable
    do_convert(do_convert(X, {type, number}, UserTypes), {type, integer}, UserTypes);
do_convert(X0, {type, integer}, _) when is_float(X0) ->
    X1 = trunc(X0),
    if X0 == X1 ->
           X1;
       true ->
           convert_error(X0, integer)
    end;
do_convert(X, {type, integer}, _) when is_integer(X) ->
    X;
do_convert(X, {type, integer}, UserTypes) when is_list(X) ->
    %% TODO: Figure out how to make base configurable
    do_convert(do_convert(X, {type, number}, UserTypes), {type, integer}, UserTypes);
%% @doc convert to list
%% @end
do_convert(X, {type, list}, _) when is_atom(X) ->
    atom_to_list(X);
do_convert(X, {type, list}, _) when is_binary(X) ->
    binary_to_list(X);
do_convert(X, {type, list}, _) when is_bitstring(X) ->
    bitstring_to_list(X);
do_convert(X, {type, list}, _) when is_float(X) ->
    float_to_list(X);
do_convert(X, {type, list}, _) when is_function(X) ->
    erlang:fun_to_list(X);
do_convert(X, {type, list}, _) when is_integer(X) ->
    integer_to_list(X);
do_convert(X, {type, list}, _) when is_map(X) ->
    maps:to_list(X);
do_convert(X, {type, list}, _) when is_pid(X) ->
    pid_to_list(X);
do_convert(X, {type, list}, _) when is_port(X) ->
    port_to_list(X);
do_convert(X, {type, list}, _) when is_reference(X) ->
    ref_to_list(X);
do_convert(X, {type, list}, UserTypes) when is_tuple(X) ->
    case get_record_spec(element(1, X), UserTypes) of
        undefined ->
            tuple_to_list(X);
        {Arity, Fields, _} when size(X) =:= Arity ->
            do_convert(X, {type, list_record, Fields}, UserTypes)
    end;
do_convert(X, {type, list_record, Fields}, UserTypes) ->
    record_to_list(list_record, X, Fields, UserTypes);
%% @doc convert to map
%% @end
do_convert(X, {type, map}, _) when is_list(X) ->
    maps:from_list(X);
do_convert(X, {type, map}, _) when is_map(X) ->
    X;
do_convert(X, {type, map}, UserTypes) when is_tuple(X) ->
    case get_record_spec(element(1, X), UserTypes) of
        {Arity, Fields, _} when size(X) =:= Arity ->
            %% This value is a record handle it a bit differently
            do_convert(X, {type, map_record, Fields}, UserTypes)
    end;
do_convert(X, {type, map_record, Fields}, UserTypes) ->
    maps:from_list(record_to_list(map_record, X, Fields, UserTypes));
%% @doc convert to number
%% @end
do_convert(X, {type, number}, _) when is_binary(X) ->
    try
        binary_to_integer(X)
    catch
        _:_ ->
            binary_to_float(X)
    end;
do_convert(X, {type, number}, _) when is_list(X) ->
    try
        list_to_integer(X)
    catch
        _:_ ->
            list_to_float(X)
    end;
%% @doc convert to pid
%% @end
do_convert(X, {type, pid}, UserTypes) when is_binary(X) ->
    do_convert(do_convert(X, {type, list}, UserTypes), {type, pid}, UserTypes);
do_convert(X, {type, pid}, _) when is_list(X) ->
    list_to_pid(X);
do_convert(X, {type, pid}, _) when is_pid(X) ->
    X;
%% @doc convert to port
%% @end
do_convert(X, {type, port}, UserTypes) when is_binary(X) ->
    do_convert(do_convert(X, {type, list}, UserTypes), {type, port}, UserTypes);
do_convert(X, {type, port}, _) when is_list(X) ->
    list_to_port(X);
do_convert(X, {type, port}, _) when is_port(X) ->
    X;
%% @doc convert to reference
%% @end
do_convert(X, {type, reference}, UserTypes) when is_binary(X) ->
    do_convert(do_convert(X, {type, list}, UserTypes), {type, reference}, UserTypes);
do_convert(X, {type, reference}, _) when is_list(X) ->
    list_to_ref(X);
do_convert(X, {type, reference}, _) when is_reference(X) ->
    X;
%% @doc convert to range value
%% @end
do_convert(X0, {type, {range, Low, High}}, UserTypes) ->
    X1 = do_convert(X0, {type, integer}, UserTypes),
    case X1 of
        _ when X1 > High orelse X1 < Low ->
            convert_error(X0, {range, Low, High});
        _ ->
            X1
    end;
%% @doc convert to union
%%      This is where conversion starts to get more complicated. Try
%%      each type in order until we successfully convert.
%% @end
do_convert(X0, {type, {union, Types}}, UserTypes) ->
    do_convert(X0, {type, {union, Types, Types}}, UserTypes);
do_convert(X0, {type, {union, Types, []}}, _) ->
    convert_error(X0, {union, Types});
do_convert(X0, {type, {union, Types, [Hd | Tl]}}, UserTypes) ->
    try
        do_convert(X0, Hd, UserTypes)
    catch
        _:_ ->
            do_convert(X0, {type, {union, Types, Tl}}, UserTypes)
    end;
%% @doc convert to tuple
%% @end
do_convert(X0, {type, {tuple, any}}, _) when is_list(X0) ->
    list_to_tuple(X0);
do_convert(X0, {type, {tuple, any}}, _) when is_tuple(X0) ->
    X0;
do_convert([], {type, {tuple, none}}, _) ->
    {};
do_convert(X0, {type, {tuple, []}}, UserTypes) ->
    do_convert(X0, {type, {tuple, any}}, UserTypes);
do_convert(X0, {type, {tuple, Types}}, UserTypes) when is_list(X0) ->
    do_convert(list_to_tuple(X0), {type, {tuple, Types}}, UserTypes);
do_convert(X0, {type, {tuple, Types}}, UserTypes) when is_tuple(X0) ->
    lists:foldl(fun({Index, Type}, Acc) ->
                    setelement(Index, Acc, do_convert(element(Index, Acc), Type, UserTypes))
                end,
                X0,
                lists:zip(lists:seq(1, length(Types)), Types));
%% @doc convert to a record
%% @end
do_convert(X0, {type, {record, _, _, _, _, _, _}} = TypeSpec, UserTypes) when is_list(X0) ->
    do_convert(maps:from_list(X0), TypeSpec, UserTypes);
do_convert(X0, {type, {record, _, Default, _, _, ArityMap, TypeMap}}, UserTypes) when is_map(X0) ->
    %% Standardize map indexes and remove entries that are not in the record
    X1 = maps:fold(fun(K0, V, X) ->
                       K1 = do_convert(K0, {type, atom}, UserTypes),
                       case maps:is_key(K1, ArityMap) of
                           true ->
                               X#{K1 => V};
                           false ->
                               X
                        end
                   end,
                   #{},
                   X0),
    maps:fold(fun(K, V0, X) ->
                  #{K := Arity} = ArityMap,
                  V1 = case maps:get(K, TypeMap, '__undefined__') of
                           '__undefined__' ->
                               V0;
                           TypeSpec ->
                               do_convert(V0, TypeSpec, UserTypes)
                       end,
                  setelement(Arity, X, V1)
              end,
              Default,
              X1);
do_convert(X0, {type, {record, Record, _, Arity, Fields, _, _}} = TypeSpec, UserTypes) when is_tuple(X0) andalso
                                                                                            element(1, X0) =:= Record andalso 
                                                                                            Arity =:= size(X0) ->
    AsList = lists:zip(Fields, tl(tuple_to_list(X0))),
    do_convert(AsList, TypeSpec, UserTypes).

%% @doc Format a conversion error
%% @end
convert_error(Value, Type) ->
    error({totype_conversion, Value, Type}).

get_record_spec(Record, UserTypes) ->
    try
        #{records := #{Record := RecordSpec}} = UserTypes,
        RecordSpec
    catch
        error:{badmatch, _} ->
            undefined
    end.

record_to_list(Nesting, X, Fields, UserTypes) ->
    Values = lists:map(fun(X0) when is_tuple(X0) ->
                              case get_record_spec(element(1, X0), UserTypes) of
                                  {Arity, Fields0, _} when size(X0) =:= Arity ->
                                      do_convert(X0, {type, Nesting, Fields0}, UserTypes);
                                  _ ->
                                      X0
                              end;
                          (X0) ->
                              X0
                       end,
                       tl(tuple_to_list(X))),
    lists:zip(Fields, Values).
