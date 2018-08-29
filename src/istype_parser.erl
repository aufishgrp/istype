-module(istype_parser).

-export([parse_type/2, parse_types/1, parse_types/2,
         parse_record/2, parse_records/1, parse_records/2, parse_records/3,
         resolve_types/1, resolve_type/2]).

-include("istype.hrl").

%%=============================================================================
%% parsing functions
%%=============================================================================
%% @doc Functions that are responsible for converting type specs and
%%      calls that represent type specs into the interlan type format.
%% @end
%%==========================================================
%% parse_types
%%==========================================================
-spec parse_types(istype:forms() | module()) -> istype:types().
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
    ParsedTypes = lists:foldl(fun({attribute, _, Class, {TypeLabel, _, TypeParams}} = Form, Acc0) when Class =:= type orelse
                                                                                                       Class =:= opaque ->
                                     {Type, Acc1} = parse_type(Module, Form, Acc0),
                                     Key = {Module, TypeLabel, length(TypeParams)},
                                     Acc1#{Key => Type};
                                 (_, Acc) ->
                                     Acc
                              end,
                              Types,
                              Forms),
    ParsedTypes.

%%=========================================================
%% parse_type_list
%%=========================================================
-spec parse_type_list(module(), istype:forms(), istype:types()) -> {list(istype:type()), istype:types()}.
parse_type_list(Module, Forms, Types) ->
    parse_type_list(fun parse_type/3, Module, Forms, Types).

parse_type_list(Fun, Module, Forms, Types) ->    
    lists:foldr(fun(Form, {TypeAcc, ExtTypeAcc0}) ->
                    {Type, ExtTypeAcc1} = Fun(Module, Form, ExtTypeAcc0),
                    {[Type | TypeAcc], ExtTypeAcc1}
                end,
                {[], Types},
                Forms).

%%==========================================================
%% parse_type/1
%%==========================================================
-spec parse_type(module(), istype:form()) -> istype:type().
%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
parse_type(Module, Form) ->
    {Type, _} = parse_type(Module, Form, #{}),
    Type.

%%==========================================================
%% parse_type/2
%%==========================================================
-spec parse_type(module(), istype:form(), istype:types()) -> {istype:type(), istype:types()}.
parse_type(Module, Form, Types) ->
    do_parse_type(Module, Form, Types).

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
do_parse_type(Module,
              {attribute, _, Class0, {_, {remote_type, _, _} = TypeSpec, TypeParams}},
              Types0) when Class0 =:= type orelse
                           Class0 =:= opaque ->
    {Type, Types1} = parse_type(Module, TypeSpec, Types0),
    {Type#type{params = TypeParams}, Types1};
do_parse_type(Module, {attribute, _, Class0, {_, TypeSpec, TypeParams}}, Types0) when Class0 =:= type orelse
                                                                                      Class0 =:= opaque ->
    Class1 = case {element(1, TypeSpec), is_bif(element(3, TypeSpec))} of
                 {atom, _} ->
                     atom;
                 {integer, _} ->
                     integer;
                 {op, _} ->
                      op;
                 {_, true} ->
                     type;
                 {_, false} ->
                     user_type
             end,
    {Type, Types1} = parse_type(Module, setelement(1, TypeSpec, Class1), Types0),
    
    {case Type of
         #type{} ->
             Type#type{params = TypeParams};
         #literal{} ->
             Type
     end,
     Types1};

%%======================================
%% Variables
%%======================================
%% @doc var '_'
%%
%%      Type variable that means any value.
%% @end
%%======================================
do_parse_type(Module, {var, _, '_'}, Types) ->
    parse_type(Module, {type, 1, any, []}, Types);
%%======================================
%% @doc Variables within type specs.
%%
%%      Type variables that need to be overridden before
%%      being considered the final type.
%% @end
%%======================================
do_parse_type(_, {var, _, _} = Var, Types) ->
    {Var, Types};
%%======================================
%% @doc Annotated Type Name :: type()
%%
%%      Type variable that means any value.
%% @end
%%======================================
do_parse_type(Module, {ann_type, _, [_, Type]}, Types) ->
    parse_type(Module, Type, Types);
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
do_parse_type(_, {nil, _} = Nil, Types) ->
    {#literal{value = Nil}, Types};
do_parse_type(_, {type, Line, nil, []}, Types) ->
    {#literal{value = {nil, Line}}, Types};
do_parse_type(_, {call, Line, {atom, _, nil}, []}, Types) ->
    {#literal{value = {nil, Line}}, Types};
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
do_parse_type(_, {atom, _, _} = Atom, Types) ->
    {#literal{value = Atom}, Types};
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
do_parse_type(_, {type, _, binary, [{integer, _, M}, {integer, _, N}]}, Types) ->
    {#type{type = bitstring,
           spec = {M, N}},
     Types};
do_parse_type(_, {bin, _, []}, Types) ->
    {#type{type = bitstring,
           spec = {0, 0}},
     Types};
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
do_parse_type(_, {type, _, 'fun', []}, Types) ->
    {#type{type = 'fun',
           spec = any},
     Types};
do_parse_type(_, {call, _, {atom, _, 'fun'}, []}, Types) ->
    {#type{type = 'fun',
           spec = any},
     Types};
do_parse_type(Module, {type, _, 'fun', [{type, _, any}, ReturnType0]}, Types0) ->
    {ReturnType1, Types1} = parse_type(Module, ReturnType0, Types0),
    {#type{type = 'fun',
           spec = {any, ReturnType1}},
     Types1};
do_parse_type(Module, {type, _, 'fun', [{type, _, product, ParameterTypes0}, ReturnType0]}, Types0) ->
    {ReturnType1, Types1} = parse_type(Module, ReturnType0, Types0),
    {ParameterTypes1, Types2} = parse_type_list(Module, ParameterTypes0, Types1),
    {#type{type = 'fun',
           spec = {ParameterTypes1, ReturnType1}},
     Types2};
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
do_parse_type(_, {integer, _, _} = Integer, Types) ->
    {#literal{value = Integer}, Types};
do_parse_type(_, {op, _, '-', {integer, _, _}} = Integer, Types) ->
    {#literal{value = Integer}, Types};
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
%% @doc Expect :: {type, _, range, [Erlang_Integer, Erlang_Integer]}
%%
%%      Ranges of Erlang_Integers.
%% @end
do_parse_type(_, {type, _, range, [Low, High]}, Types) ->
    {#type{type = range,
           spec = {#literal{value = Low}, #literal{value = High}}},
     Types};
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
do_parse_type(_, {type, _, list, [{type, _, any, []}]}, Types) ->
    {#type{type = list,
           spec = any},
     Types};
do_parse_type(Module, {type, _, list, [ValueType0]}, Types0) ->
    {ValueType1, Types1} = parse_type(Module, ValueType0, Types0),
    {#type{type = list,
           spec = {maybe_empty,
                     ValueType1,
                     parse_type(Module, {type, 1, nil, []})}},
     Types1};
do_parse_type(Module, {type, _, maybe_improper_list, [_, _] = ValueTypes0}, Types0) ->
    {ValueTypes1, Types1} = parse_type_list(Module, ValueTypes0, Types0),
    {#type{type = list,
           spec = list_to_tuple([maybe_empty | ValueTypes1])},
     Types1};
do_parse_type(Module, {type, _, nonempty_improper_list, ValueTypes0}, Types0) ->
    {ValueTypes1, Types1} = parse_type_list(Module, ValueTypes0, Types0),
    {#type{type = list,
           spec = list_to_tuple([nonempty | ValueTypes1])},
     Types1};
do_parse_type(Module, {type, _, nonempty_list, [ValueType0]}, Types0) ->
    {ValueType1, Types1} = parse_type(Module, ValueType0, Types0),
    {#type{type = list,
           spec = {nonempty, ValueType1, parse_type(Module, {type, 1, nil, []})}},
     Types1};
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
do_parse_type(_, {map, _, []}, Types) ->
    {#type{type = map,
           spec = empty},
     Types};
do_parse_type(_, {type, _, map, any}, Types) ->
    {#type{type = map,
           spec = any},
     Types};
do_parse_type(_, {call, _, {atom, _, map}, []}, Types) ->
    {#type{type = map,
           spec = any},
     Types};
do_parse_type(_, {type, _, map, []}, Types) ->
    {#type{type = map,
           spec = empty},
     Types};
do_parse_type(Module, {type, _, map, MapFields}, Types0) ->
    {MapFieldTypes, Types1} = parse_map_fields(Module, MapFields, Types0),
    {#type{type = map,
           spec = MapFieldTypes},
     Types1};
%%======================================
%% Tuple
%%======================================
%% @doc Expect :: {type, _, tuple, any}           %% Any Tuple
%%              | {call, _, {atom, _, tuple}, []} %% Any Tuple
%%              | {type, _, tuple, []}            %% Empty Tuple
%%              | {type, _, tuple, FieldTypes}    %% Typed Tuple
%%              | {tuple, _, _}                   %% Tuple literal
%% @end
do_parse_type(_, {tuple, _, []}, Types) ->
    {#type{type = tuple,
           spec = empty},
     Types};
do_parse_type(Module, {tuple, _, FieldTypes0}, Types0) ->
    {FieldTypes1, Types1} = parse_tuple_fields(Module, FieldTypes0, Types0),
    {#type{type = tuple,
           spec = FieldTypes1},
     Types1};
do_parse_type(_, {type, _, tuple, any}, Types) ->
    {#type{type = tuple,
           spec = any},
     Types};
do_parse_type(_, {call, _, {atom, _, tuple}, []}, Types) ->
    {#type{type = tuple,
           spec = any},
     Types};
do_parse_type(_, {type, _, tuple, []}, Types) ->
    {#type{type = tuple,
           spec = empty},
     Types};
do_parse_type(Module, {type, _, tuple, FieldTypes0}, Types0) ->
    {FieldTypes1, Types1} = parse_tuple_fields(Module, FieldTypes0, Types0),
    {#type{type = tuple,
           spec = FieldTypes1},
     Types1};
%%======================================
%% Union
%%======================================
%% @doc Expect :: {type, _, union, Types} %% Any Tuple
%% @end
do_parse_type(Module, {type, _, union, UnionTypes0}, Types0) ->
    {UnionTypes1, Types1} = parse_type_list(Module, UnionTypes0, Types0),
    {#type{type = union,
           spec = UnionTypes1},
     Types1};
%%======================================
%% term()
%%======================================
%% @doc Expect :: {type, _, term, []}
%%              | {call, _, {atom, _, term}, []}
%%      Alias for any().
%%
%%      Calls are handled by the default call handler.
%% @end
do_parse_type(Module, {type, Line, term, []}, Types) ->
    parse_type(Module, {type, Line, any, []}, Types);
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
do_parse_type(Module, {type, Line, byte, []}, Types) ->
    parse_type(Module,
               {type, Line, range, [{integer, Line, 0},
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
do_parse_type(Module, {type, Line, char, []}, Types) ->
    parse_type(Module, 
               {type, Line, range, [{integer, Line, 0},
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
do_parse_type(Module, {type, Line, list, []}, Types) ->
    parse_type(Module, {type, Line, list, [{type, Line, any, []}]}, Types);
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
do_parse_type(Module, {type, Line, maybe_improper_list, []}, Types) ->
    parse_type(Module, {type, Line, maybe_improper_list, [{type, Line, any, []}, {type, Line, any, []}]}, Types);
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
do_parse_type(Module, {type, Line, nonempty_list, []}, Types) ->
    parse_type(Module, {type, Line, nonempty_list, [{type, Line, any, []}]}, Types);
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
do_parse_type(Module, {type, Line, string, []}, Types) ->
    parse_type(Module, {type, Line, list, [{type, Line, char, []}]}, Types);
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
do_parse_type(Module, {type, Line, nonempty_string, []}, Types) ->
    parse_type(Module, {type, Line, nonempty_list, [{type, Line, char, []}]}, Types);
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
do_parse_type(Module, {type, Line, iodata, []}, Types) ->
    parse_type(Module,
               {type, Line, union, [{type, Line, iolist, []},
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
do_parse_type(_, {type, _, iolist, []}, Types) ->
    {#type{type = iolist}, Types};
do_parse_type(_, {type, iolist, []}, Types) ->
    {#type{type = iolist}, Types};
%%======================================
%% function()
%%======================================
%% @doc Expect :: {type, _, function, []}
%%              | {call, _, {atom, _, function}, []}
%%
%%      Alias for fun().
%% @end
do_parse_type(Module, {type, Line, function, []}, Types) ->
    parse_type(Module, {type, Line, 'fun', []}, Types);
do_parse_type(Module, {call, Line, {atom, _, function}, []}, Types) ->
    parse_type(Module, {call, Line, {atom, Line, 'fun'}, []}, Types);
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
do_parse_type(Module, {type, Line, module, []}, Types) ->
    parse_type(Module, {type, Line, atom, []}, Types);
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
do_parse_type(Module, {type, Line, mfa, []}, Types) ->
    parse_type(Module, 
               {type, Line, tuple, [{type, Line, module, []},
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
do_parse_type(Module, {type, Line, arity, []}, Types) ->
    parse_type(Module, 
               {type, Line, range, [{integer, Line, 0},
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
do_parse_type(Module, {type, Line, identifier, []}, Types) ->
    parse_type(Module, 
               {type, Line, union, [{type, Line, pid, []},
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
do_parse_type(Module, {type, Line, node, []}, Types) ->
    parse_type(Module, {type, Line, atom, []}, Types);
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
do_parse_type(Module, {type, Line, timeout, []}, Types) ->
    parse_type(Module,
               {type, Line, union, [{atom, Line, 'infinity'},
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
do_parse_type(Module, {type, Line, no_return, []}, Types) ->
    parse_type(Module, {type, Line, none, []}, Types);
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
do_parse_type(Module, {type, Line, non_neg_integer, []}, Types) ->
    parse_type(Module,
               {type, Line, range, [{integer, Line, 0},
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
do_parse_type(Module, {type, Line, pos_integer, []}, Types) ->
    parse_type(Module, 
               {type, Line, range, [{integer, Line, 1},
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
do_parse_type(Module, {type, Line, neg_integer, []}, Types) ->
    parse_type(Module,
               {type, Line, range, [{atom, Line, undefined},
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
do_parse_type(Module, {type, Line, nonempty_maybe_improper_list, []}, Types) ->
    parse_type(Module,
               {type, Line, nonempty_maybe_improper_list, [{type, Line, any, []},
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
do_parse_type(Module, {type, _, nonempty_maybe_improper_list, ValueTypes0}, Types0) ->
    {ValueTypes1, Types1} = parse_type_list(Module, ValueTypes0, Types0),
    {#type{type = list,
           spec = list_to_tuple([nonempty | ValueTypes1])},
     Types1};
%%======================================
%% Record
%%======================================
%% @doc Expect :: {type, _, record, [Record | FieldOverrides]}
%%              | {record, _, Record, FieldOverrides}
%% @end
do_parse_type(Module, {type, _, record, [{atom, _, Record} | RecordFields0]}, Types0) ->
    ParseFun = fun(M, {type, _, field_type, [{atom, _, Field}, FieldType0]}, TypesAcc0) ->
                   {FieldType1, TypesAcc1} = parse_type(M, FieldType0, TypesAcc0),
                   {{Field, FieldType1}, TypesAcc1}
               end,

    {RecordFields1, Types1} = parse_type_list(ParseFun, Module, RecordFields0, Types0),
    {#type{type = record,
           spec = {Record, RecordFields1}},
     Types1};
do_parse_type(Module, {record, _, Record, RecordFields}, Types) ->
    Fields = lists:map(fun parse_record_field/1, RecordFields),
    FieldTypes = lists:map(fun({record_field, _, _, Type0}) ->
                               {Type1, _} = parse_type(Module, Type0, Types),
                               Type1
                           end,
                           RecordFields),
    {#type{type = record,
           spec = {Record, lists:zip(Fields, FieldTypes)}},
     Types};

%%======================================
%% Default call handler
%%======================================
do_parse_type(Module, {call, Line, {atom, _, Type}, TypeArgs}, Types) ->
    parse_type(Module, {type, Line, Type, TypeArgs}, Types);
%%======================================
%% Remote call handler
%%======================================
do_parse_type(_, {call, Line, {remote, _, Module, Type}, TypeArgs}, Types) ->
    parse_type(Module, {remote_type, Line, [Module, Type, TypeArgs]}, Types);
%%======================================
%% Remote handler
%%======================================
do_parse_type(_, {remote_type, _, RemoteTypeSpec} = TypeSpec, Types0) ->
    [{atom, _, Module},
     {atom, _, Type},
     TypeParams] = RemoteTypeSpec,

    TypeKey = {Module, Type, length(TypeParams)},
    ParsedKey = {parsed, Module},

    case Types0 of
        #{TypeKey := RemoteType} ->
            {RemoteType, Types0};
        #{ParsedKey := true} ->
            parse_error({type_not_found, TypeKey});
        _ ->
            Types1 = parse_types(Module),
            Types2 = maps:merge(Types0#{ParsedKey => true}, Types1),
            parse_type(Module, TypeSpec, Types2)
    end;
%%======================================
%% Default handlers
%%======================================
do_parse_type(Module, {user_type, _, Type, TypeArgs}, Types0) ->
    {TList, Types1} = parse_type_list(Module, TypeArgs, Types0),
    {#type{module = Module,
           type   = Type,
           spec   = TList},
     Types1};
do_parse_type(Module0, {type, _, Type, TypeArgs}, Types0) ->
    {TList, Types1} = parse_type_list(Module0, TypeArgs, Types0),
    Module1 = case is_bif(Type) of
                  true ->
                      erlang;
                  false ->
                      Module0
              end,
    {#type{module = Module1,
           type   = Type,
           spec   = TList},
     Types1};
do_parse_type(_, Type, _) ->
    parse_error({unparsed, Type}).

%%=========================================================
%% parse_error
%%=========================================================
parse_error(Type) ->
    error({parse_type, Type}).

%%=========================================================
%% parse_map_fields
%%=========================================================
parse_map_fields(Module, Fields0, Types0) ->
    {Exact, Types1} = lists:foldr(fun(Types, {FieldAcc, TypeAcc0}) ->
                                      {Fields, TypeAcc1} = parse_type_list(Module, Types, TypeAcc0),
                                      {[list_to_tuple(Fields) | FieldAcc], TypeAcc1}
                                  end,
                                  {[], Types0},
                                  [F || {type, _, map_field_exact, F} <- Fields0]),
    {Assoc, Types2} = lists:foldr(fun(Types, {FieldAcc, TypeAcc0}) ->
                                      {Fields, TypeAcc1} = parse_type_list(Module, Types, TypeAcc0),
                                      {[list_to_tuple(Fields) | FieldAcc], TypeAcc1}
                                  end,
                                  {[], Types1},
                                  [F || {type, _, map_field_assoc, F} <- Fields0]),
    {{Exact, Assoc}, Types2}.

%%=========================================================
%% parse_tuple_fields
%%=========================================================
parse_tuple_fields(Module, Forms, Types0) ->
    {TypeList, Types1} = parse_type_list(Module, Forms, Types0),
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
    {attribute, _, module, Module} = lists:keyfind(module, 3, Forms),
    lists:foldl(fun({attribute, _, record, {RecordLabel, _}} = Form, {TypesAcc0, RecordsAcc}) ->
                        {Record, TypesAcc1} = parse_record(Module, Form, TypesAcc0),
                        {TypesAcc1, RecordsAcc#{RecordLabel => Record}};
                    (_, Acc) ->
                        Acc
                end,
                {Types, Records},
                Forms).

%%=========================================================
%% parse_record/1
%%=========================================================
-spec parse_record(module(), istype:form()) -> istype:record().
parse_record(Module, Form) ->
    {Record, _} = parse_record(Module, Form, #{}),
    Record.

%%=========================================================
%% parse_record/2
%%=========================================================
-spec parse_record(module(), istype:form(), istype:types()) -> {istype:record(), istype:types()}.
parse_record(Module, {attribute, _, record, {Record, RecordFields}}, Types0) ->
    {{Arity, Fields, FieldTypes, FieldDefaults}, Types1} = parse_record_fields(Module, RecordFields, Types0),
    
    {#record{arity    = Arity,
             record   = Record,
             fields   = Fields,
             types    = FieldTypes,
             defaults = FieldDefaults},
     Types1}.

%%=========================================================
%% parse_record_fields
%%=========================================================
parse_record_fields(Module, RecordFields, Types0) ->
    Arity = length(RecordFields) + 1,
    Fields = lists:map(fun parse_record_field/1, RecordFields),
    FieldDefaults = lists:map(fun parse_record_field_default/1, RecordFields),
    {FieldTypes, Types1} = lists:foldr(fun(RecordField, {RecordFieldAcc, TypesAcc0}) ->
                                           {ParsedRecordField, TypesAcc1} = parse_record_field_type(Module, RecordField, TypesAcc0),
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

%% @doc Extract the default from a record field.
%% @end
parse_record_field_default({typed_record_field, RecordField, _}) ->
    parse_record_field_default(RecordField);
parse_record_field_default({record_field, Line, _}) ->
    #literal{value = {atom, Line, undefined}};
parse_record_field_default({record_field, _, _, Default}) ->
    #literal{value = Default}.

%% @doc Extract the type from a record field.
%% @end
parse_record_field_type(Module, {typed_record_field, _, Type}, Types) ->
    parse_type(Module, Type, Types);
parse_record_field_type(Module, _, Types) ->
    parse_type(Module, {type, 1, any, []}, Types).

%%=========================================================
%% is_bif
%%=========================================================
is_bif(any) -> true;
is_bif(none) -> true;
is_bif(nil) -> true;
is_bif(pid) -> true;
is_bif(port) -> true;
is_bif(reference) -> true;
is_bif(atom) -> true;
is_bif('fun') -> true;
is_bif(float) -> true;
is_bif(integer) -> true;
is_bif(range) -> true;
is_bif(list) -> true;
is_bif(nonempty_maybe_improper_list) -> true;
is_bif(maybe_improper_list) -> true;
is_bif(nonempty_improper_list) -> true;
is_bif(nonempty_list) -> true;
is_bif(map) -> true;
is_bif(tuple) -> true;
is_bif(union) -> true;
is_bif(term) -> true;
is_bif(binary) -> true;
is_bif(bitstring) -> true;
is_bif(boolean) -> true;
is_bif(byte) -> true;
is_bif(char) -> true;
is_bif(number) -> true;
is_bif(string) -> true;
is_bif(nonempty_string) -> true;
is_bif(iodata) -> true;
is_bif(iolist) -> true;
is_bif(function) -> true;
is_bif(module) -> true;
is_bif(mfa) -> true;
is_bif(arity) -> true;
is_bif(identifier) -> true;
is_bif(node) -> true;
is_bif(timeout) -> true;
is_bif(no_return) -> true;
is_bif(non_neg_integer) -> true;
is_bif(pos_integer) -> true;
is_bif(neg_integer) -> true;
is_bif(record) -> true;
is_bif(_) -> false.

%%=========================================================
%% resolve_types
%%=========================================================
-spec resolve_types(istype:types()) -> istype:types().
%% @doc Resolves the parameterized types into thier primitives.
%% @end
resolve_types(Types0) ->
    Types1 = maps:fold(fun(K, #type{} = V0, Acc) ->
                              V1 = resolve_type(V0, Types0),
                              Acc#{K => V1#type{params = V0#type.params}};
                          (_, _, Acc) ->
                              Acc
                       end,
                       #{},
                       Types0),
    maps:merge(Types0, Types1).

resolve_type(#type{spec = Spec} = Base, Types) when is_list(Spec) ->
    MFA = {Base#type.module,
           Base#type.type,
           length(Spec)},
    case Types of
        #{MFA := Type0} ->
            Type1 = do_resolve_type(Base, Type0, Types),
            case Type1 of
                #type{} ->
                    Type1#type{params = Base#type.params};
                _ ->
                    Type1
            end;
        _ -> 
            Base
    end;
resolve_type(Base, _) ->
  Base.

do_resolve_type(Base, #type{} = Type, Types) ->
    ParamKeys = lists:map(fun({var, _, V}) ->
                                 {var, V};
                             (V) ->
                                 V
                          end,
                          Type#type.params),
    
    ParamMap = maps:from_list(lists:zip(ParamKeys, Base#type.spec)),
    
    Spec0 = istype_transform:update_term(fun Fun({var, _, Var}) ->
                                                    maps:get({var, Var}, ParamMap);
                                             Fun(#type{} = T) ->
                                                    T#type{spec = istype_transform:update_term(Fun, T#type.spec)};
                                             Fun(X) -> 
                                                    X
                                         end,
                                         Type#type.spec),
    Spec1 = istype_transform:update_term(fun(#type{} = TypeX) ->
                                                resolve_type(TypeX, Types);
                                            (X) ->
                                                X
                                         end,
                                         Spec0),
    resolve_type(Type#type{spec = Spec1}, Types);
do_resolve_type(_, #literal{} = Literal, _) ->
    Literal.
