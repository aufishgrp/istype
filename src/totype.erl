-module(totype).
-export([convert/2]).

%% @doc Master conversion function.
%%      Attempts to convert X into a type as specified by typedata.
%%      In the event that X is a tuple/record all typed fields are
%%      recursively converted.
%%      In the event of a union the first successful conversion is returned.
%% @end
convert(Value, Type) ->
    try
        do_convert(Value, Type)
    catch
        error:Error when element(1, Error) =:= totype_conversion ->
            error(Error);
        _:_ ->
            convert_error(Value, Type)
    end.

do_convert(X, {literal, Y}) when is_atom(Y) ->
    Y = do_convert(X, {type, atom});
do_convert(X, {literal, Y}) when is_integer(Y) ->
    Y = do_convert(X, {type, integer});

%% @doc Convert to atom
%% @end
do_convert(X, {type, atom}) when is_atom(X) ->
    X;
do_convert(X, {type, atom}) when is_binary(X) ->
    %% TODO: Figure out how to make existing and encoding configurable
    binary_to_existing_atom(X, utf8);
do_convert(X, {type, atom}) when is_list(X) ->
    list_to_existing_atom(X);
%% @doc convert to binary
%% @end
do_convert(X, {type, binary}) when is_atom(X) ->
    %% TODO: Figure out how to make encoding configurable
    atom_to_binary(X, utf8);
do_convert(X, {type, binary}) when is_binary(X) ->
    X;
do_convert(X, {type, binary}) when is_float(X) ->
    %% TODO: Figure out how to make option configurable
    float_to_binary(X);
do_convert(X, {type, binary}) when is_integer(X) ->
    integer_to_binary(X);
do_convert(X, {type, binary}) when is_list(X) ->
    list_to_binary(X);
do_convert(X, {type, binary}) when is_function(X) orelse
                                   is_pid(X) orelse
                                   is_port(X) orelse
                                   is_reference(X) ->
    do_convert(do_convert(X, {type, list}), {type, binary});
%% @doc convert to bitstring
%% @end
do_convert(X, {type, bitstring}) when is_bitstring(X) ->
    X;
do_convert(X, {type, bitstring}) when is_list(X) ->
    list_to_bitstring(X);
%% @doc convert to boolean
%% @end
do_convert(X, {type, boolean}) when is_atom(X) andalso
                                    (X =:= true orelse
                                     X =:= false) ->
    X;
do_convert(X, {type, boolean}) when is_binary(X) ->
    do_convert(do_convert(X, {type, atom}), {type, boolean});
do_convert(X, {type, boolean}) when is_list(X) ->
    do_convert(do_convert(X, {type, atom}), {type, boolean});
%% @doc float
do_convert(X, {type, float}) when is_binary(X) ->
    do_convert(do_convert(X, {type, number}), {type, float});
do_convert(X, {type, float}) when is_float(X) ->
    X;
do_convert(X, {type, float}) when is_integer(X) ->
    X * 1.0;
do_convert(X, {type, float}) when is_list(X) ->
    do_convert(do_convert(X, {type, number}), {type, float});
%% @doc convert to integer
%% @end
do_convert(X, {type, integer}) when is_binary(X) ->
    %% TODO: Figure out how to make base configurable
    do_convert(do_convert(X, {type, number}), {type, integer});
do_convert(X0, {type, integer}) when is_float(X0) ->
    X1 = trunc(X0),
    if X0 == X1 ->
           X1;
       true ->
           convert_error(X0, integer)
    end;
do_convert(X, {type, integer}) when is_integer(X) ->
    X;
do_convert(X, {type, integer}) when is_list(X) ->
    %% TODO: Figure out how to make base configurable
    do_convert(do_convert(X, {type, number}), {type, integer});
%% @doc convert to list
%% @end
do_convert(X, {type, list}) when is_atom(X) ->
    atom_to_list(X);
do_convert(X, {type, list}) when is_binary(X) ->
    binary_to_list(X);
do_convert(X, {type, list}) when is_bitstring(X) ->
    bitstring_to_list(X);
do_convert(X, {type, list}) when is_float(X) ->
    float_to_list(X);
do_convert(X, {type, list}) when is_function(X) ->
    erlang:fun_to_list(X);
do_convert(X, {type, list}) when is_integer(X) ->
    integer_to_list(X);
do_convert(X, {type, list}) when is_map(X) ->
    maps:to_list(X);
do_convert(X, {type, list}) when is_pid(X) ->
    pid_to_list(X);
do_convert(X, {type, list}) when is_port(X) ->
    port_to_list(X);
do_convert(X, {type, list}) when is_reference(X) ->
    ref_to_list(X);
do_convert(X, {type, list}) when is_tuple(X) ->
    tuple_to_list(X);
%% @doc convert to map
%% @end
do_convert(X, {type, map}) when is_list(X) ->
    maps:from_list(X);
do_convert(X, {type, map}) when is_map(X) ->
    X;
%% @doc convert to number
%% @end
do_convert(X, {type, number}) when is_binary(X) ->
    try
        binary_to_integer(X)
    catch
        _:_ ->
            binary_to_float(X)
    end;
do_convert(X, {type, number}) when is_list(X) ->
    try
        list_to_integer(X)
    catch
        _:_ ->
            list_to_float(X)
    end;
%% @doc convert to pid
%% @end
do_convert(X, {type, pid}) when is_binary(X) ->
    do_convert(do_convert(X, {type, list}), {type, pid});
do_convert(X, {type, pid}) when is_list(X) ->
    list_to_pid(X);
do_convert(X, {type, pid}) when is_pid(X) ->
    X;
%% @doc convert to port
%% @end
do_convert(X, {type, port}) when is_binary(X) ->
    do_convert(do_convert(X, {type, list}), {type, port});
do_convert(X, {type, port}) when is_list(X) ->
    list_to_port(X);
do_convert(X, {type, port}) when is_port(X) ->
    X;
%% @doc convert to reference
%% @end
do_convert(X, {type, reference}) when is_binary(X) ->
    do_convert(do_convert(X, {type, list}), {type, reference});
do_convert(X, {type, reference}) when is_list(X) ->
    list_to_ref(X);
do_convert(X, {type, reference}) when is_reference(X) ->
    X;
%% @doc convert to range value
%% @end
do_convert(X0, {type, {range, Low, High}}) ->
    X1 = do_convert(X0, {type, integer}),
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
do_convert(X0, {type, {union, Types}}) ->
    do_convert(X0, {type, {union, Types, Types}});
do_convert(X0, {type, {union, Types, []}}) ->
    convert_error(X0, {union, Types});
do_convert(X0, {type, {union, Types, [Hd | Tl]}}) ->
    try
        do_convert(X0, Hd)
    catch
        _:_ ->
            do_convert(X0, {type, {union, Types, Tl}})
    end;
%% @doc convert to tuple
%% @end
do_convert(X0, {type, {tuple, any}}) when is_list(X0) ->
    list_to_tuple(X0);
do_convert(X0, {type, {tuple, any}}) when is_tuple(X0) ->
    X0;
do_convert([], {type, {tuple, none}}) ->
    {};
do_convert(X0, {type, {tuple, []}}) ->
    do_convert(X0, {type, {tuple, any}});
do_convert(X0, {type, {tuple, Types}}) when is_list(X0) ->
    do_convert(list_to_tuple(X0), {type, {tuple, Types}});
do_convert(X0, {type, {tuple, Types}}) when is_tuple(X0) ->
    lists:foldl(fun({Index, Type}, Acc) ->
                    setelement(Index, Acc, do_convert(element(Index, Acc), Type))
                end,
                X0,
                lists:zip(lists:seq(1, length(Types)), Types)).

%% @doc Format a conversion error
%% @end
convert_error(Value, Type) ->
    error({totype_conversion, Value, Type}).

