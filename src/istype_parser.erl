-module(istype_parser).

-export([parse_types/1, parse_types/2, parse_type/1]).

-type form()      :: erl_parse:abstract_form().
-type forms()     :: list(form()).
-type type()      :: {type, atom(), any | tuple() | list()} |
                     {record, atom(), list()}.
-type types()     :: #{atom() => type()}.
-type ext_types() :: #{module() => types()}.

-export_types([form/0, forms/0, type/0, types/0]).

%%=============================================================================
%% parsing functions
%%=============================================================================
%% @doc Functions that are responsible for converting type specs and
%%      calls that represent type specs into the interlan type format.
%% @end

-spec parse_types(forms()) -> types().
%%==========================================================
%% parse_types
%%==========================================================
%% @doc Converts a types, calls representing types, and literals
%%      into the internal type format.
%% @end
parse_types(Forms) ->
    TypesData = lists:foldl(fun({attribute, _, type, {TypeLabel, _, _}} = Form, {Types, ExtTypes0}) ->
                                   {Type, ExtTypes1} = parse_type(Form, ExtTypes0),
                                   {Types#{TypeLabel => Type}, ExtTypes1};
                               (_, Acc) ->
                                   Acc
                            end,
                            {#{}, #{}},
                            Forms),
    {Types, _} = TypesData,
    Types.

-spec parse_types(forms(), ext_types()) -> {list(type()), ext_types()}.
parse_types(Forms, ExtTypes0) ->
    parse_types(Forms, [], ExtTypes0).

-spec parse_types(forms(), list(type()), ext_types()) -> {list(type()), ext_types()}.
parse_types([], Parsed, ExtTypes) ->
    {lists:reverse(Parsed), ExtTypes};
parse_types([Form | Forms], Parsed, ExtTypes0) ->
    {Type, ExtTypes1} = parse_type(Form, ExtTypes0),
    parse_types(Forms, [Type | Parsed], ExtTypes1).

-spec parse_type(form()) -> type().
%%==========================================================
%% parse_type/1
%%==========================================================
%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
parse_type(Type) ->
    {Typing, _} = parse_type(Type, #{}),
    Typing.

-spec parse_type(form(), ext_types()) -> {type(), ext_types()}.
%%==========================================================
%% parse_type/2
%%==========================================================
%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
%%======================================
%% @doc attribute
%%
%%      Form that wraps the type spec.
%% @end
%%======================================
parse_type({attribute, _, type, {_, TypeSpec, _}}, ExtTypes) ->
    parse_type(TypeSpec, ExtTypes);
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
parse_type({nil, _} = Nil, ExtTypes) ->
    {{literal, Nil}, ExtTypes};
parse_type({type, Line, nil, []}, ExtTypes) ->
    {{literal, {nil, Line}}, ExtTypes};
parse_type({call, Line, {atom, _, nil}, []}, ExtTypes) ->
    {{literal, {nil, Line}}, ExtTypes};
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
parse_type({atom, _, _} = Atom, ExtTypes) ->
    {{literal, Atom}, ExtTypes};
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
parse_type({type, _, binary, [{integer, _, M}, {integer, _, N}]}, ExtTypes) ->
    {{type, bitstring, {M, N}}, ExtTypes};
parse_type({bin, _, []}, ExtTypes) ->
    {{type, bitstring, {0, 0}}, ExtTypes};
parse_type({bin, _, _} = Bitstring, ExtTypes) ->
    {{literal, Bitstring}, ExtTypes};
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
%%              | {type, _, 'fun', [{type, _, product, ParameterExtTypes}, ReturnType]}
%% @end
parse_type({type, _, 'fun', []}, ExtTypes) ->
    {{type, 'fun', any}, ExtTypes};
parse_type({call, _, {atom, _, 'fun'}, []}, ExtTypes) ->
    {{type, 'fun', any}, ExtTypes};
parse_type({type, _, 'fun', [{type, _, any}, ReturnType0]}, ExtTypes0) ->
    {ReturnType1, ExtTypes1} = parse_type(ReturnType0, ExtTypes0),
    {{type, 'fun', {any, ReturnType1}}, ExtTypes1};
parse_type({type, _, 'fun', [{type, _, product, ParameterTypes0}, ReturnType0]}, ExtTypes0) ->
    {ReturnType1, ExtTypes1} = parse_type(ReturnType0, ExtTypes0),
    {ParameterTypes1, ExtTypes2} = parse_types(ParameterTypes0, ExtTypes1),
    {{type, 'fun', {ParameterTypes1, ReturnType1}}, ExtTypes2};
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
parse_type({integer, _, _} = Integer, ExtTypes) ->
    {{literal, Integer}, ExtTypes};
parse_type({op, _, '-', {integer, _, _}} = Integer, ExtTypes) ->
    {{literal, Integer}, ExtTypes};
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
%% @doc Expect :: {type, _, range, [Erlang_Integer, Erlang_Integer]}
%%
%%      Ranges of Erlang_Integers.
%% @end
parse_type({type, _, range, [Low, High]}, ExtTypes) ->
    {{type, range, {{literal, Low}, {literal, High}}}, ExtTypes};
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
parse_type({type, _, list, [{type, _, any, []}]}, ExtTypes) ->
    {{type, list, any}, ExtTypes};
parse_type({type, _, list, [ValueType0]}, ExtTypes0) ->
    {ValueType1, ExtTypes1} = parse_type(ValueType0, ExtTypes0),
    {{type, list, {maybe_empty, ValueType1, parse_type({type, 1, nil, []})}}, ExtTypes1};
parse_type({type, _, maybe_improper_list, [_, _] = ValueTypes0}, ExtTypes0) ->
    {ValueTypes1, ExtTypes1} = parse_types(ValueTypes0, ExtTypes0),
    {{type, list, list_to_tuple([maybe_empty | ValueTypes1])}, ExtTypes1};
parse_type({type, _, nonempty_improper_list, ValueTypes0}, ExtTypes0) ->
    {ValueTypes1, ExtTypes1} = parse_types(ValueTypes0, ExtTypes0),
    {{type, list, list_to_tuple([nonempty | ValueTypes1])}, ExtTypes1};
parse_type({type, _, nonempty_list, [ValueType0]}, ExtTypes0) ->
    {ValueType1, ExtTypes1} = parse_type(ValueType0, ExtTypes0),
    {{type, list, {nonempty, ValueType1, parse_type({type, 1, nil, []})}}, ExtTypes1};
%%======================================
%% Map
%%======================================
%% @doc Expect :: {map, _, MapFields}           %% Literal
%%              | {type, _, map, any}           %% Any Map
%%              | {call, _, {atom, _, map}, []} %% Any Map
%%              | {type, _, map, []}            %% Empty Map
%%              | {type, _, map, MapFields}     %% Typed Map
%%
%%      MapFields :: list({type, _, map_field_assoc, ExtTypes})
%%                 | list({type, _, map_field_exact, ExtTypes})
%%
%%      A call to map() is treated as any map.
%% @end
parse_type({type, _, map, any}, ExtTypes) ->
    {{type, map, any}, ExtTypes};
parse_type({call, _, {atom, _, map}, []}, ExtTypes) ->
    {{type, map, any}, ExtTypes};
parse_type({type, _, map, []}, ExtTypes) ->
    {{type, map, empty}, ExtTypes};
parse_type({type, _, map, MapFields}, ExtTypes0) ->
    {MapFieldTypes, ExtTypes1} = parse_map_fields(MapFields, ExtTypes0),
    {{type, map, MapFieldTypes}, ExtTypes1};
%%======================================
%% Tuple
%%======================================
%% @doc Expect :: {type, _, tuple, any}           %% Any Tuple
%%              | {call, _, {atom, _, tuple}, []} %% Any Tuple
%%              | {type, _, tuple, []}            %% Empty Tuple
%%              | {type, _, tuple, FieldExtTypes}    %% Typed Tuple
%%              | {tuple, _, _}                   %% Tuple literal
%% @end
parse_type({type, _, tuple, any}, ExtTypes) ->
    {{type, tuple, any}, ExtTypes};
parse_type({call, _, {atom, _, tuple}, []}, ExtTypes) ->
    {{type, tuple, any}, ExtTypes};
parse_type({type, _, tuple, FieldTypes0}, ExtTypes0) ->
    {FieldTypes1, ExtTypes1} = parse_types(FieldTypes0, ExtTypes0),
    {{type, tuple, FieldTypes1}, ExtTypes1};
parse_type({tuple, _, _} = Tuple, ExtTypes) ->
    {{literal, Tuple}, ExtTypes};
%%======================================
%% Union
%%======================================
%% @doc Expect :: {type, _, union, ExtTypes} %% Any Tuple
%% @end
parse_type({type, _, union, UnionTypes0}, ExtTypes0) ->
    {UnionTypes1, ExtTypes1} = parse_types(UnionTypes0, ExtTypes0),
    {{type, union, UnionTypes1}, ExtTypes1};
%%======================================
%% term()
%%======================================
%% @doc Expect :: {type, _, term, []}
%%              | {call, _, {atom, _, term}, []}
%%      Alias for any().
%%
%%      Calls are handled by the default call handler.
%% @end
parse_type({type, Line, term, []}, ExtTypes) ->
    parse_type({type, Line, any, []}, ExtTypes);
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
parse_type({type, Line, byte, []}, ExtTypes) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 255}]},
               ExtTypes);
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
parse_type({type, Line, char, []}, ExtTypes) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 16#10ffff}]},
               ExtTypes);
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
parse_type({type, Line, list, []}, ExtTypes) ->
    parse_type({type, Line, list, [{type, Line, any, []}]}, ExtTypes);
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
parse_type({type, Line, maybe_improper_list, []}, ExtTypes) ->
    parse_type({type, Line, maybe_improper_list, [{type, Line, any, []}, {type, Line, any, []}]}, ExtTypes);
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
parse_type({type, Line, nonempty_list, []}, ExtTypes) ->
    parse_type({type, Line, nonempty_list, [{type, Line, any, []}]}, ExtTypes);
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
parse_type({type, Line, string, []}, ExtTypes) ->
    parse_type({type, Line, list, [{type, Line, char, []}]}, ExtTypes);
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
parse_type({type, Line, nonempty_string, []}, ExtTypes) ->
    parse_type({type, Line, nonempty_list, [{type, Line, char, []}]}, ExtTypes);
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
parse_type({type, Line, iodata, []}, ExtTypes) ->
    parse_type({type, Line, union, [{type, Line, iolist, []},
                                    {type, Line, binary, []}]},
               ExtTypes);
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
parse_type({type, Line, iolist, []}, ExtTypes) ->
    Type1 = {type, Line, union, [{type, Line, byte, []},
                                 {type, Line, binary, []},
                                 {type, iolist, []}]},
    Type2 = {type, Line, union, [{type, Line, binary, []},
                                 {type, Line, nil, []}]},
    parse_type({type, Line, maybe_improper_list, [Type1, Type2]}, ExtTypes);
parse_type({type, iolist, []} = Iolist, ExtTypes) ->
    {Iolist, ExtTypes};
%%======================================
%% function()
%%======================================
%% @doc Expect :: {type, _, function, []}
%%              | {call, _, {atom, _, function}, []}
%%
%%      Alias for fun().
%%      All cases can be handled by the default handler.
%% @end
parse_type({type, Line, function, []}, ExtTypes) ->
    parse_type({type, Line, 'fun', []}, ExtTypes);
parse_type({call, Line, function, []}, ExtTypes) ->
    parse_type({call, Line, 'fun', []}, ExtTypes);
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
parse_type({type, Line, module, []}, ExtTypes) ->
    parse_type({type, Line, atom, []}, ExtTypes);
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
parse_type({type, Line, mfa, []}, ExtTypes) ->
    parse_type({type, Line, tuple, [{type, Line, module, []},
                                    {type, Line, atom, []},
                                    {type, Line, arity, []}]},
               ExtTypes);
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
parse_type({type, Line, arity, []}, ExtTypes) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 255}]},
               ExtTypes);
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
parse_type({type, Line, identifier, []}, ExtTypes) ->
    parse_type({type, Line, union, [{type, Line, pid, []},
                                    {type, Line, port, []},
                                    {type, Line, reference, []}]},
               ExtTypes);
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
parse_type({type, Line, node, []}, ExtTypes) ->
    parse_type({type, Line, atom, []}, ExtTypes);
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
parse_type({type, Line, timeout, []}, ExtTypes) ->
    parse_type({type, Line, union, [{atom, Line, 'infinity'},
                                    {type, Line, non_neg_integer, []}]},
               ExtTypes);
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
parse_type({type, Line, no_return, []}, ExtTypes) ->
    parse_type({type, Line, none, []}, ExtTypes);
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
parse_type({type, Line, non_neg_integer, []}, ExtTypes) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {atom, Line, undefined}]},
               ExtTypes);
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
parse_type({type, Line, pos_integer, []}, ExtTypes) ->
    parse_type({type, Line, range, [{integer, Line, 1},
                                    {atom, Line, undefined}]},
               ExtTypes);
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
parse_type({type, Line, neg_integer, []}, ExtTypes) ->
    parse_type({type, Line, range, [{atom, Line, undefined},
                                    {op, Line, '-', {integer, Line, 1}}]},
               ExtTypes);
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
parse_type({type, Line, nonempty_maybe_improper_list, []}, ExtTypes) ->
    parse_type({type, Line, nonempty_maybe_improper_list, [{type, Line, any, []},
                                                           {type, Line, any, []}]},
               ExtTypes);
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
parse_type({type, _, nonempty_maybe_improper_list, ValueTypes0}, ExtTypes0) ->
    {ValueTypes1, ExtTypes1} = parse_types(ValueTypes0, ExtTypes0),    
    {{type, list, list_to_tuple([nonempty | ValueTypes1])}, ExtTypes1};
%%======================================
%% Record
%%======================================
%% @doc Expect :: {type, _, record, [Record | FieldOverrides]}
%%              | {record, _, Record, FieldOverrides}
%% @end
parse_type({type, _, record, [{atom, _, Record} | RecordFields0]}, ExtTypes) ->
    RecordFields1 = lists:map(fun({type, _, field_type, [{atom, _, Field}, FieldType]}) ->
                                  {Field, parse_type(FieldType)}
                              end,
                              RecordFields0),
    {{record, Record, RecordFields1}, ExtTypes};
parse_type({record, _, Record, RecordFields}, ExtTypes) ->
    {{record, Record, parse_literal_record_fields(RecordFields)}, ExtTypes};
%%======================================
%% Default call handler
%%======================================
parse_type({call, Line, {atom, _, Type}, TypeArgs}, ExtTypes) ->
    parse_type({type, Line, Type, TypeArgs}, ExtTypes);
%%======================================
%% Remote call handler
%%======================================
parse_type({call, Line, {remote, _, Module, Type}, TypeArgs}, ExtTypes) ->
    parse_type({remote_type, Line, [Module, Type, TypeArgs]}, ExtTypes);
%%======================================
%% Remote handler
%%======================================
parse_type({remote_type, _, RemoteType} = TypeSpec, ExtTypes) ->
    [{atom, _, Module},
     {atom, _, Type},
     _] = RemoteType,

    case ExtTypes of
        #{Module := RemoteTypes} ->
            case RemoteTypes of
                #{Type := Typing} ->
                    {Typing, ExtTypes};
                _ ->
                    parse_error({invalid_remote_type, Module, Type})
            end;
        _ ->
            parse_type(TypeSpec, ExtTypes#{Module => parse_types(forms:read(Module))})
    end;
%%======================================
%% Default handler
%%======================================
parse_type({Class, _, Type, TypeArgs}, ExtTypes) when Class =:= type orelse
                                                      Class =:= user_type ->
    {{type, Type, lists:map(parse_type_fun(ExtTypes), TypeArgs)}, ExtTypes};
parse_type(Type, _) ->
    parse_error(Type).

%%=========================================================
%% parse_error
%%=========================================================
parse_error(Type) ->
    error({parse_type, Type}).

%%=========================================================
%% parse_map_fields
%%=========================================================
parse_map_fields(Fields0, ExtTypes0) ->
    {Exact, ExtTypes1} = lists:foldl(fun(ExtTypes, {FieldAcc, TypeAcc0}) ->
                                      {Fields, TypeAcc1} = parse_types(ExtTypes, TypeAcc0),
                                      {[list_to_tuple(Fields) | FieldAcc], TypeAcc1}
                                  end,
                                  {[], ExtTypes0},
                                  [F || {type, _, map_field_exact, F} <- Fields0]),
    {Assoc, ExtTypes2} = lists:foldl(fun(ExtTypes, {FieldAcc, TypeAcc0}) ->
                                      {Fields, TypeAcc1} = parse_types(ExtTypes, TypeAcc0),
                                      {[list_to_tuple(Fields) | FieldAcc], TypeAcc1}
                                  end,
                                  {[], ExtTypes1},
                                  [F || {type, _, map_field_assoc, F} <- Fields0]),
    {{lists:reverse(Exact), lists:reverse(Assoc)}, ExtTypes2}.

%%=========================================================
%% parse_record
%%=========================================================
parse_record_fields(RecordFields) ->
    Arity = length(RecordFields) + 1,
    Fields = lists:map(fun parse_record_field/1, RecordFields),
    FieldExtTypes = lists:map(fun parse_record_field_type/1, RecordFields),
    {Arity, Fields, FieldExtTypes}.

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

%% @doc A literal record value is given as a type. Get it's definition and
%%      update it with any overrides given.
%% @end
parse_literal_record_fields(RecordFields) ->
    %% Fetch overrides
    OverrideFields = lists:map(fun parse_literal_record_field/1, RecordFields),
    OverrideFieldTypes = lists:map(fun parse_literal_record_field_type/1, RecordFields),
    lists:zip(OverrideFields, OverrideFieldTypes).

%% @doc Extract the name from a record field.
%% @end
parse_literal_record_field({typed_record_field, RecordField, _}) ->
    parse_literal_record_field(RecordField);
parse_literal_record_field({record_field, _, {atom, _, RecordField}}) ->
    RecordField;
parse_literal_record_field({record_field, _, {atom, _, RecordField}, _}) ->
    RecordField.

%% @doc Extract the type from a record field.
%% @end
parse_literal_record_field_type({record_field, _, _, Type}) ->
    parse_type(Type);
parse_literal_record_field_type(_) ->
    parse_type({type, 1, any, []}).

parse_type_fun(ExtTypes) ->
    fun(Type) ->
        parse_type(Type, ExtTypes)
    end.
