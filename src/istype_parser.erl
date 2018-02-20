-module(istype_parser).

-export([parse_types/1, parse_types/2,
         parse_records/1, parse_records/2, parse_records/3, parse_record/1]).

-ifdef(EUNIT).
-export([parse_type/1,
         type/1, type/2, type/3,
         literal/1]).
-endif.


%%=============================================================================
%% parsing functions
%%=============================================================================
%% @doc Functions that are responsible for converting type specs and
%%      calls that represent type specs into the interlan type format.
%% @end
%%==========================================================
%% parse_types
%%==========================================================
-spec parse_types(istype:forms()) -> istype:types().
%% @doc Converts a types, calls representing types, and literals
%%      into the internal type format.
%% @end
parse_types(Forms) ->
    parse_types(Forms, #{}).

-spec parse_types(istype:forms() | module(), istype:types()) -> istype:types().
parse_types(Module, Types) when is_atom(Module) ->
    parse_types(forms:read(Module), Types);
parse_types(Forms, Types) ->
    {attribute, _, module, Module} = lists:keyfind(module, 3, Forms),
    lists:foldl(fun({attribute, _, Class, {TypeLabel, _, TypeParams}} = Form, Acc0) when Class =:= type orelse
                                                                                         Class =:= opaque ->
                       {Type, Acc1} = parse_type(Form, Acc0),
                       Key = {Module, TypeLabel, length(TypeParams)},
                       Acc1#{Key => Type};
                   (_, Acc) ->
                       Acc
                end,
                Types,
                Forms).

%%=========================================================
%% parse_type_list
%%=========================================================
-spec parse_type_list(istype:forms(), istype:types()) -> {list(istype:type()), istype:types()}.
parse_type_list(Forms, Types) ->
    lists:foldr(fun(Form, {TypeAcc, ExtTypeAcc0}) ->
                    {Type, ExtTypeAcc1} = parse_type(Form, ExtTypeAcc0),
                    {[Type | TypeAcc], ExtTypeAcc1}
                end,
                {[], Types},
                Forms).

%%==========================================================
%% parse_type/1
%%==========================================================
-spec parse_type(istype:form()) -> istype:type().
%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
parse_type(Form) ->
    {Type, _} = parse_type(Form, #{}),
    Type.

%%==========================================================
%% parse_type/2
%%==========================================================
-spec parse_type(istype:form(), istype:types()) -> {istype:type(), istype:types()}.
parse_type(Form, Types) ->
    io:format("\nParse Type\n~p\n", [Form]),
    do_parse_type(Form, Types).

%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
%%======================================
%% Attributes
%%======================================
%% @doc Expect :: {attribute, _, type, {_, TypeSpec, _}} |
%%                {attribute, _, opaque, {_, TypeSpec, _}}
%%
%%      Form that wraps the type spec.
%% @end
%%======================================
do_parse_type({attribute, _, type, {_, TypeSpec, TypeParams}}, Types0) ->
    {Type, Types1} = parse_type(TypeSpec, Types0),
    {set_type_params(TypeParams, Type), Types1};
do_parse_type({attribute, _, opaque, {_, TypeSpec, TypeParams}}, Types0) ->
    {Type, Types1} = parse_type(TypeSpec, Types0),
    {set_type_params(TypeParams, Type), Types1};
%%======================================
%% Variables
%%======================================
%% @doc var '_'
%%
%%      Type variable that means any value.
%% @end
%%======================================
do_parse_type({var, _, '_'}, Types) ->
    parse_type({type, 1, any, []}, Types);
%%======================================
%% @doc Variables within type specs.
%%
%%      Type variables that need to be overridden before
%%      being considered the final type.
%% @end
%%======================================
do_parse_type({var, _, _} = Var, Types) ->
    {Var, Types};
%%======================================
%% @doc Annotated Type Name :: type()
%%
%%      Type variable that means any value.
%% @end
%%======================================
do_parse_type({ann_type, _, [_, Type]}, Types) ->
    parse_type(Type, Types);
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
%%      Nil refers to a specific value. Treat it as
%%      a literal.
%% @end
do_parse_type({nil, _} = Nil, Types) ->
    {literal(Nil), Types};
do_parse_type({type, Line, nil, []}, Types) ->
    {literal({nil, Line}), Types};
do_parse_type({call, Line, {atom, _, nil}, []}, Types) ->
    {literal({nil, Line}), Types};
%%======================================
%% Atom
%%======================================
%% atom()
%%==================
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
do_parse_type({atom, _, _} = Atom, Types) ->
    {literal(Atom), Types};
%%======================================
%% Bitstring
%%======================================
%% @doc Expect :: {type, _, binary, [{integer, _, M}, {integer, _, N}]}
%%              | {bin, _, []}
%%              | {bin, _, [...]}
%%
%%      These patterns represent specific bitstrings formats.
%%      They need to be treated as literal values and should only
%%      be found within type specs.
%% @end
do_parse_type({type, _, binary, [{integer, _, M}, {integer, _, N}]}, Types) ->
    {type(bitstring, {M, N}), Types};
do_parse_type({bin, _, []}, Types) ->
    {type(bitstring, {0, 0}), Types};
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
%% @end
do_parse_type({type, _, 'fun', []}, Types) ->
    {type('fun', any), Types};
do_parse_type({call, _, {atom, _, 'fun'}, []}, Types) ->
    {type('fun', any), Types};
do_parse_type({type, _, 'fun', [{type, _, any}, ReturnType0]}, Types0) ->
    {ReturnType1, Types1} = parse_type(ReturnType0, Types0),
    {type('fun', {any, ReturnType1}), Types1};
do_parse_type({type, _, 'fun', [{type, _, product, ParameterTypes0}, ReturnType0]}, Types0) ->
    {ReturnType1, Types1} = parse_type(ReturnType0, Types0),
    {ParameterTypes1, Types2} = parse_type_list(ParameterTypes0, Types1),
    {type('fun', {ParameterTypes1, ReturnType1}), Types2};
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
do_parse_type({integer, _, _} = Integer, Types) ->
    {literal(Integer), Types};
do_parse_type({op, _, '-', {integer, _, _}} = Integer, Types) ->
    {literal(Integer), Types};
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
%% @doc Expect :: {type, _, range, [Erlang_Integer, Erlang_Integer]}
%%
%%      Ranges of Erlang_Integers.
%% @end
do_parse_type({type, _, range, [Low, High]}, Types) ->
    {type(range, {{literal, Low}, {literal, High}}), Types};
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
%%      Calls handled by the default call handler.
%% @end
do_parse_type({type, _, list, [{type, _, any, []}]}, Types) ->
    {type(list, any), Types};
do_parse_type({type, _, list, [ValueType0]}, Types0) ->
    {ValueType1, Types1} = parse_type(ValueType0, Types0),
    {type(list, {maybe_empty, ValueType1, parse_type({type, 1, nil, []})}), Types1};
do_parse_type({type, _, maybe_improper_list, [_, _] = ValueTypes0}, Types0) ->
    {ValueTypes1, Types1} = parse_type_list(ValueTypes0, Types0),
    {type(list, list_to_tuple([maybe_empty | ValueTypes1])), Types1};
do_parse_type({type, _, nonempty_improper_list, ValueTypes0}, Types0) ->
    {ValueTypes1, Types1} = parse_type_list(ValueTypes0, Types0),
    {type(list, list_to_tuple([nonempty | ValueTypes1])), Types1};
do_parse_type({type, _, nonempty_list, [ValueType0]}, Types0) ->
    {ValueType1, Types1} = parse_type(ValueType0, Types0),
    {type(list, {nonempty, ValueType1, parse_type({type, 1, nil, []})}), Types1};
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
do_parse_type({type, _, map, any}, Types) ->
    {type(map, any), Types};
do_parse_type({call, _, {atom, _, map}, []}, Types) ->
    {type(map, any), Types};
do_parse_type({type, _, map, []}, Types) ->
    {type(map, empty), Types};
do_parse_type({type, _, map, MapFields}, Types0) ->
    {MapFieldTypes, Types1} = parse_map_fields(MapFields, Types0),
    {type(map, MapFieldTypes), Types1};
%%======================================
%% Tuple
%%======================================
%% @doc Expect :: {type, _, tuple, any}           %% Any Tuple
%%              | {call, _, {atom, _, tuple}, []} %% Any Tuple
%%              | {type, _, tuple, []}            %% Empty Tuple
%%              | {type, _, tuple, FieldTypes}    %% Typed Tuple
%%              | {tuple, _, _}                   %% Tuple literal
%% @end
do_parse_type({type, _, tuple, any}, Types) ->
    {type(tuple, any), Types};
do_parse_type({call, _, {atom, _, tuple}, []}, Types) ->
    {type(tuple, any), Types};
do_parse_type({type, _, tuple, []}, Types) ->
    {type(tuple, empty), Types};
do_parse_type({type, _, tuple, FieldTypes0}, Types0) ->
    {FieldTypes1, Types1} = parse_tuple_fields(FieldTypes0, Types0),
    {type(tuple, FieldTypes1), Types1};
%%======================================
%% Union
%%======================================
%% @doc Expect :: {type, _, union, Types} %% Any Tuple
%% @end
do_parse_type({type, _, union, UnionTypes0}, Types0) ->
    {UnionTypes1, Types1} = parse_type_list(UnionTypes0, Types0),
    {type(union, UnionTypes1), Types1};
%%======================================
%% term()
%%======================================
%% @doc Expect :: {type, _, term, []}
%%              | {call, _, {atom, _, term}, []}
%%      Alias for any().
%%
%%      Calls are handled by the default call handler.
%% @end
do_parse_type({type, Line, term, []}, Types) ->
    parse_type({type, Line, any, []}, Types);
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
do_parse_type({type, Line, byte, []}, Types) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 255}]},
               Types);
%%======================================
%% char()
%%======================================
%% @doc Expect :: {type, _, char, []}
%%              | {call, _, {atom, _, char}, []}
%%
%%      Alias for 0..16#10ffff.
%%
%%      Calls are handled by the default call handler.
%% @end
do_parse_type({type, Line, char, []}, Types) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 16#10ffff}]},
               Types);
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
do_parse_type({type, Line, list, []}, Types) ->
    parse_type({type, Line, list, [{type, Line, any, []}]}, Types);
%%======================================
%% maybe_improper_list()
%%======================================
%% @doc Expect :: {type, _, maybe_improper_list, []}
%%              | {call, _, {atom, _, maybe_improper_list}, []}
%%
%%      Alias for maybe_improper_list(any(), any()).
%%
%%      Calls handled by the default call handler.
%% @end
do_parse_type({type, Line, maybe_improper_list, []}, Types) ->
    parse_type({type, Line, maybe_improper_list, [{type, Line, any, []}, {type, Line, any, []}]}, Types);
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
do_parse_type({type, Line, nonempty_list, []}, Types) ->
    parse_type({type, Line, nonempty_list, [{type, Line, any, []}]}, Types);
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
do_parse_type({type, Line, string, []}, Types) ->
    parse_type({type, Line, list, [{type, Line, char, []}]}, Types);
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
do_parse_type({type, Line, nonempty_string, []}, Types) ->
    parse_type({type, Line, nonempty_list, [{type, Line, char, []}]}, Types);
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
do_parse_type({type, Line, iodata, []}, Types) ->
    parse_type({type, Line, union, [{type, Line, iolist, []},
                                    {type, Line, binary, []}]},
               Types);
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
do_parse_type({type, Line, iolist, []}, Types) ->
    Form1 = {type, Line, union, [{type, Line, byte, []},
                                 {type, Line, binary, []},
                                 {type, iolist, []}]},
    Form2 = {type, Line, union, [{type, Line, binary, []},
                                 {type, Line, nil, []}]},
    parse_type({type, Line, maybe_improper_list, [Form1, Form2]}, Types);
do_parse_type({type, iolist, []}, Types) ->
    {type(iolist, []), Types};
%%======================================
%% function()
%%======================================
%% @doc Expect :: {type, _, function, []}
%%              | {call, _, {atom, _, function}, []}
%%
%%      Alias for fun().
%% @end
do_parse_type({type, Line, function, []}, Types) ->
    parse_type({type, Line, 'fun', []}, Types);
do_parse_type({call, Line, {atom, _, function}, []}, Types) ->
    parse_type({call, Line, {atom, Line, 'fun'}, []}, Types);
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
do_parse_type({type, Line, module, []}, Types) ->
    parse_type({type, Line, atom, []}, Types);
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
do_parse_type({type, Line, mfa, []}, Types) ->
    parse_type({type, Line, tuple, [{type, Line, module, []},
                                    {type, Line, atom, []},
                                    {type, Line, arity, []}]},
               Types);
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
do_parse_type({type, Line, arity, []}, Types) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 255}]},
               Types);
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
do_parse_type({type, Line, identifier, []}, Types) ->
    parse_type({type, Line, union, [{type, Line, pid, []},
                                    {type, Line, port, []},
                                    {type, Line, reference, []}]},
               Types);
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
do_parse_type({type, Line, node, []}, Types) ->
    parse_type({type, Line, atom, []}, Types);
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
do_parse_type({type, Line, timeout, []}, Types) ->
    parse_type({type, Line, union, [{atom, Line, 'infinity'},
                                    {type, Line, non_neg_integer, []}]},
               Types);
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
do_parse_type({type, Line, no_return, []}, Types) ->
    parse_type({type, Line, none, []}, Types);
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
do_parse_type({type, Line, non_neg_integer, []}, Types) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {atom, Line, undefined}]},
               Types);
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
do_parse_type({type, Line, pos_integer, []}, Types) ->
    parse_type({type, Line, range, [{integer, Line, 1},
                                    {atom, Line, undefined}]},
               Types);
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
do_parse_type({type, Line, neg_integer, []}, Types) ->
    parse_type({type, Line, range, [{atom, Line, undefined},
                                    {op, Line, '-', {integer, Line, 1}}]},
               Types);
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
do_parse_type({type, Line, nonempty_maybe_improper_list, []}, Types) ->
    parse_type({type, Line, nonempty_maybe_improper_list, [{type, Line, any, []},
                                                           {type, Line, any, []}]},
               Types);
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
do_parse_type({type, _, nonempty_maybe_improper_list, ValueTypes0}, Types0) ->
    {ValueTypes1, Types1} = parse_type_list(ValueTypes0, Types0),
    {type(list, list_to_tuple([nonempty | ValueTypes1])), Types1};
%%======================================
%% Record
%%======================================
%% @doc Expect :: {type, _, record, [Record | FieldOverrides]}
%%              | {record, _, Record, FieldOverrides}
%% @end
do_parse_type({type, _, record, [{atom, _, Record} | RecordFields0]}, Types) ->
    RecordFields1 = lists:map(fun({type, _, field_type, [{atom, _, Field}, FieldType]}) ->
                                  {Field, parse_type(FieldType)}
                              end,
                              RecordFields0),
    {type(record, {Record, RecordFields1}), Types};
%%======================================
%% Default call handler
%%======================================
do_parse_type({call, Line, {atom, _, Type}, TypeArgs}, Types) ->
    parse_type({type, Line, Type, TypeArgs}, Types);
%%======================================
%% Remote call handler
%%======================================
do_parse_type({call, Line, {remote, _, Module, Type}, TypeArgs}, Types) ->
    parse_type({remote_type, Line, [Module, Type, TypeArgs]}, Types);
%%======================================
%% Remote handler
%%======================================
do_parse_type({remote_type, _, RemoteTypeSpec} = TypeSpec, Types0) ->
    [{atom, _, Module},
     {atom, _, Type},
     _] = RemoteTypeSpec,

    TypeKey = {Module, Type},
    ParsedKey = {parsed, Module},

    case Types0 of
        #{TypeKey := RemoteType} ->
            {RemoteType, Types0};
        #{ParsedKey := true} ->
            io:format("Could not find ~p\n~p\n", [TypeKey, Types0]),
            {parse_type({type, 1, any, []}), Types0};
        _ ->
            io:format("Parse Module ~p\n", [Module]),
            Types1 = parse_types(Module),
            Types2 = maps:fold(fun(K, V, Acc) when is_atom(K) ->
                                      Acc#{{Module, K} => V};
                                  (_, _, Acc) ->
                                      Acc
                               end,
                               Types0#{{parsed, Module} => true},
                               Types1),
            parse_type(TypeSpec, Types2)
    end;
%%======================================
%% Default handler
%%======================================
do_parse_type({Class, _, Type, TypeArgs}, Types0) when Class =:= type orelse
                                                       Class =:= user_type ->

    {TList, Types1} = parse_type_list(TypeArgs, Types0),
    {type(Type, TList), Types1};
do_parse_type(Type, _) ->
    parse_error(Type).

%%=========================================================
%% parse_error
%%=========================================================
parse_error(Type) ->
    io:format("Cound not parse\n~p\n", [Type]),
    error({parse_type, Type}).

%%=========================================================
%% parse_map_fields
%%=========================================================
parse_map_fields(Fields0, Types0) ->
    {Exact, Types1} = lists:foldr(fun(Types, {FieldAcc, TypeAcc0}) ->
                                      {Fields, TypeAcc1} = parse_type_list(Types, TypeAcc0),
                                      {[list_to_tuple(Fields) | FieldAcc], TypeAcc1}
                                  end,
                                  {[], Types0},
                                  [F || {type, _, map_field_exact, F} <- Fields0]),
    {Assoc, Types2} = lists:foldr(fun(Types, {FieldAcc, TypeAcc0}) ->
                                      {Fields, TypeAcc1} = parse_type_list(Types, TypeAcc0),
                                      {[list_to_tuple(Fields) | FieldAcc], TypeAcc1}
                                  end,
                                  {[], Types1},
                                  [F || {type, _, map_field_assoc, F} <- Fields0]),
    {{Exact, Assoc}, Types2}.

%%=========================================================
%% parse_tuple_fields
%%=========================================================
parse_tuple_fields(Forms, Types0) ->
    {TypeList, Types1} = parse_type_list(Forms, Types0),
    Length = length(Forms),
    {{Length, lists:zip(lists:seq(1, Length), TypeList)}, Types1}.

%%=========================================================
%% parse_records
%%=========================================================
-spec parse_records(istype:forms()) -> istype:records().
parse_records(Forms) ->
    {_, Records} = parse_records(Forms, #{}, #{}),
    Records.

-spec parse_records(istype:forms(), istype:types()) -> {istype:types(), istype:records()}.
parse_records(Forms, Types) ->
    parse_records(Forms, Types, #{}).

-spec parse_records(istype:forms(), istype:types(), istype:records()) -> {istype:types(), istype:records()}.
%% @doc
%% @end
parse_records(Forms, Types, Records) ->
    lists:foldl(fun({attribute, _, record, {RecordLabel, _}} = Form, {TypesAcc0, RecordsAcc}) ->
                        {Record, TypesAcc1} = parse_record(Form, TypesAcc0),
                        {TypesAcc1, RecordsAcc#{RecordLabel => Record}};
                    (_, Acc) ->
                        Acc
                end,
                {Types, Records},
                Forms).

%%=========================================================
%% parse_record/1
%%=========================================================
-spec parse_record(istype:form()) -> istype:record().
parse_record(Form) ->
    {Record, _} = parse_record(Form, #{}),
    Record.

%%=========================================================
%% parse_record/2
%%=========================================================
-spec parse_record(istype:form(), istype:types()) -> {istype:record(), istype:types()}.
parse_record({attribute, _, record, {Record, RecordFields}}, Types0) ->
    {{Arity, Fields, FieldTypes, FieldDefaults}, Types1} = parse_record_fields(RecordFields, Types0),
    {{record, Record, Arity, Fields, FieldTypes, FieldDefaults}, Types1}.

%%=========================================================
%% parse_record_fields
%%=========================================================
parse_record_fields(RecordFields, Types0) ->
    Arity = length(RecordFields) + 1,
    Fields = lists:map(fun parse_record_field/1, RecordFields),
    FieldDefaults = lists:map(fun parse_record_field_default/1, RecordFields),
    {FieldTypes, Types1} = lists:foldr(fun(RecordField, {RecordFieldAcc, TypesAcc0}) ->
                                           {ParsedRecordField, TypesAcc1} = parse_record_field_type(RecordField, TypesAcc0),
                                           {[ParsedRecordField | RecordFieldAcc], TypesAcc1}
                                       end,
                                       {[], Types0},
                                       RecordFields),
    {{Arity,
      Fields,
      maps:from_list(lists:zip(Fields, FieldTypes)),
      maps:from_list(lists:zip(Fields, FieldDefaults))},
     Types1}.

%% @doc Extract the name from a record field.
%% @end
parse_record_field({typed_record_field, RecordField, _}) ->
    parse_record_field(RecordField);
parse_record_field({record_field, _, {atom, _, RecordField}}) ->
    RecordField;
parse_record_field({record_field, _, {atom, _, RecordField}, _}) ->
    RecordField.

%% @end
parse_record_field_default({typed_record_field, RecordField, _}) ->
    parse_record_field_default(RecordField);
parse_record_field_default({record_field, Line, _}) ->
    literal({atom, Line, undefined});
parse_record_field_default({record_field, _, _, Default}) ->
    literal(Default).

%% @doc Extract the type from a record field.
%% @end
parse_record_field_type({typed_record_field, _, Type}, Types) ->
    parse_type(Type, Types);
parse_record_field_type(_, Types) ->
    parse_type({type, 1, any, []}, Types).

%%=========================================================
%% type
%%=========================================================
-spec type(atom()) -> istype:type().
type(Type) ->
    type(Type, []).

-spec type(atom(), istype:typespec()) -> istype:type().
type(Type, TypeSpec) ->
    type(Type, TypeSpec, []).

-spec type(atom(), istype:typespec(), list()) -> istype:type().
type(Type, TypeSpec, TypeParams) ->
    {type, Type, TypeSpec, TypeParams}.

-spec set_type_params(istype:forms(), istype:type()) -> istype:type().
set_type_params(TypeParams, {type, Type, TypeSpec, _}) ->
    {type, Type, TypeSpec, TypeParams}.

-spec get_type_params(istype:type()) -> istype:forms().
get_type_params({type, _, _, Params}) ->
    Params.

%%=========================================================
%% literal
%%=========================================================
-spec literal(istype:form()) -> istype:literal().
literal(Form) ->
    {literal, Form}.
