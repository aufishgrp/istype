-module(istype_parser).

-export([parse_types/1,
         parse_type/1]).

-export([parse_records/1, parse_records/3,
         parse_record/1]).

-record(state, {
    module         = undefined  :: module(),
    types          = #{}        :: istype:types(),
    records        = #{}        :: istype:records(),
    options        = #{}        :: istype:options(),
    parameters     = []         :: list(istype:type()),
    parsed_modules = sets:new() :: sets:set()
    
}).

-type state() :: #state{}.

%%=============================================================================
%% parsing api functions
%%=============================================================================
%% parse_types/1
%%==========================================================
-spec parse_types(istype:forms() | module()) -> istype:types().
%% @doc Converts a types, calls representing types, and literals
%%      into the internal type format.
%% @end
parse_types(Forms) ->
    State = parse_types(Forms, #state{}),
    State#state.types.

%%==========================================================
%% parse_type/1
%%==========================================================
-spec parse_type(istype:form()) -> istype:type().
%% @doc Converts a Type, calls representing a type, and literals
%%      into the internal type format.
%% @end
parse_type(Form) ->
    {Type, _} = parse_type(Form, #state{}),
    Type.

%%=============================================================================
%% parsing functions
%%=============================================================================
%% @doc Functions that are responsible for converting type specs and
%%      calls that represent type specs into the interlan type format.
%% @end
%%==========================================================
%% parse_type/2
%%==========================================================
-spec parse_types(istype:forms() | module(), state()) -> state().
parse_types(Module, State) when is_atom(Module) ->
    Forms = forms:read(Module),
    do_parse_types(Forms, State#state{module = Module});
parse_types(Forms, #state{module = undefined} = State) when is_list(Forms) ->
    {_, _, module, Module} = lists:keyfind(module, 3, Forms),
    do_parse_types(Forms, State#state{module = Module});
parse_types(Forms, #state{} = State) when is_list(Forms) ->
    do_parse_types(Forms, State).

%%=========================================================
%% do_parse_types/2
%%=========================================================
-spec do_parse_types(istype:forms(), state()) -> state().
do_parse_types(Forms, State) ->
    lists:foldl(fun({attribute, _, Class, {Label, _, Args}} = TypeSpec, Acc0) when Class =:= type orelse
                                                                                   Class =:= opaque ->
                       {Type, Acc1} = parse_type(TypeSpec, Acc0),
                       store_type(Label, length(Args), Type, Acc1);
                   ({call, _, {atom, _, istype}, [_, Type]}, Acc0) ->
                       {ParsedType, Acc1} = parse_type(Type, Acc0),
                       {type, Label, Args} = ParsedType,
                       store_type(Label, length(Args), ParsedType, Acc1);
                   (_, Acc) ->
                       Acc
                end,
                State,
                Forms).

%%=========================================================
%% parse_type_list
%%=========================================================
-spec parse_type_list(istype:forms(), state()) -> {list(istype:type()), state()}.
parse_type_list(Forms, State) ->
    lists:foldr(fun(Form, {TypesAcc, StateAcc0}) ->
                    {Type, StateAcc1} = parse_type(Form, State),
                    {[Type | TypesAcc], StateAcc1}
                end,
                {[], State},
                Forms).

%%==========================================================
%% parse_type/2
%%==========================================================
-spec parse_type(istype:form(), state()) -> {istype:type(), state()}.
parse_type(Form, State) ->
    %io:format("\nParse Type\n~p\n", [Form]),
    X = do_parse_type(Form, State),
    %io:format("Result\n~p\n", [X]),
    X.

%%==========================================================
%% do_parse_type/2
%%==========================================================
%% Attributes
%%======================================
%% @doc Expect :: {attribute, _, type, {_, TypeSpec, _}} |
%%                {attribute, _, opaque, {_, TypeSpec, _}}
%%
%%      Form that wraps the type spec.
%% @end
%%======================================
do_parse_type({attribute, _, Class, {_, TypeSpec, Parameters}}, State0) when Class =:= type orelse
                                                                             Class =:= opaque ->
    
    Args0 = case TypeSpec of
                {_, _, _, TypeArgs} ->
                    TypeArgs;
                {_, _, [_, _, RemoteTypeArgs]} ->
                    RemoteTypeArgs
            end,
    io:format("Parsing ~p\n", [TypeSpec]),
    {{type, Label, Args1} = Y, State1} = parse_type(TypeSpec, State0),
    io:format("Parsed ~p\n", [Y]),


    OrderedParameters = lists:zip(lists:seq(1, length(Parameters)), Parameters),
    Args2 = lists:map(fun(Arg) ->
                          case lists:keyfind(Arg, 2, OrderedParameters) of
                              false ->
                                  Arg;
                              {X, _} ->
                                  X
                          end
                      end,
                      Args1),
    {{type, Label, Args2}, State1};

%%======================================
%% Variables
%%======================================
%% @doc var '_'
%%
%%      Type variable that means any value.
%% @end
%%======================================
do_parse_type({var, _, '_'}, State) ->
    parse_type({type, 1, any, []}, State);
%%======================================
%% @doc Variables within type specs.
%%
%%      Type variables that need to be overridden before
%%      being considered the final type.
%% @end
%%======================================
do_parse_type({var, _, _} = Var, State) ->
    {Var, State};
%%======================================
%% @doc Annotated Type Name :: type()
%%
%%      Type variable that means any value.
%% @end
%%======================================
do_parse_type({ann_type, _, [_, Type]}, State) ->
    parse_type(Type, State);
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
do_parse_type({nil, _} = Nil, State) ->
    {{literal, Nil}, State};
do_parse_type({type, Line, nil, []}, State) ->
    {{literal, {nil, Line}}, State};
do_parse_type({call, Line, {atom, _, nil}, []}, State) ->
    {{literal, {nil, Line}}, State};
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
do_parse_type({atom, _, _} = Atom, State) ->
    {{literal, Atom}, State};
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
do_parse_type({type, _, binary, [{integer, _, M}, {integer, _, N}]}, State) ->
    {{type, bitstring, {M, N}}, State};
do_parse_type({bin, _, []}, State) ->
    {{type, bitstring, {0, 0}}, State};
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
do_parse_type({type, _, 'fun', []}, State) ->
    {{type, 'fun', any}, State};
do_parse_type({call, _, {atom, _, 'fun'}, []}, State) ->
    {{type, 'fun', any}, State};
do_parse_type({type, _, 'fun', [{type, _, any}, ReturnType0]}, State0) ->
    {ReturnType1, State1} = parse_type(ReturnType0, State0),
    {{type, 'fun', {any, ReturnType1}}, State1};
do_parse_type({type, _, 'fun', [{type, _, product, ParameterTypes0}, ReturnType0]}, State0) ->
    {ReturnType1, State1} = parse_type(ReturnType0, State0),
    {ParameterTypes1, State2} = parse_type_list(ParameterTypes0, State1),
    {{type, 'fun', {ParameterTypes1, ReturnType1}}, State2};
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
do_parse_type({integer, _, _} = Integer, State) ->
    {{literal, Integer}, State};
do_parse_type({op, _, '-', {integer, _, _}} = Integer, State) ->
    {{literal, Integer}, State};
%%==================
%% Erlang_Integer..Erlang_Integer
%%==================
%% @doc Expect :: {type, _, range, [Erlang_Integer, Erlang_Integer]}
%%
%%      Ranges of Erlang_Integers.
%% @end
do_parse_type({type, _, range, [Low, High]}, State) ->
    {{type, range, {{literal, Low}, {literal, High}}}, State};
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
do_parse_type({type, _, list, [{type, _, any, []}]}, State) ->
    {{type, list, any}, State};
do_parse_type({type, _, list, [ValueType0]}, State0) ->
    {ValueType1, State1} = parse_type(ValueType0, State0),
    {{type, list, {maybe_empty, ValueType1, parse_type({type, 1, nil, []})}}, State1};
do_parse_type({type, _, maybe_improper_list, [_, _] = ValueTypes0}, State0) ->
    {ValueTypes1, State1} = parse_type_list(ValueTypes0, State0),
    {{type, list, list_to_tuple([maybe_empty | ValueTypes1])}, State1};
do_parse_type({type, _, nonempty_improper_list, ValueTypes0}, State0) ->
    {ValueTypes1, State1} = parse_type_list(ValueTypes0, State0),
    {{type, list, list_to_tuple([nonempty | ValueTypes1])}, State1};
do_parse_type({type, _, nonempty_list, [ValueType0]}, State0) ->
    {ValueType1, State1} = parse_type(ValueType0, State0),
    {{type, list, {nonempty, ValueType1, parse_type({type, 1, nil, []})}}, State1};
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
do_parse_type({type, _, map, any}, State) ->
    {{type, map, any}, State};
do_parse_type({call, _, {atom, _, map}, []}, State) ->
    {{type, map, any}, State};
do_parse_type({type, _, map, []}, State) ->
    {{type, map, empty}, State};
do_parse_type({type, _, map, MapFields}, State0) ->
    {MapFieldTypes, State1} = parse_map_fields(MapFields, State0),
    {{type, map, MapFieldTypes}, State1};
%%======================================
%% Tuple
%%======================================
%% @doc Expect :: {type, _, tuple, any}           %% Any Tuple
%%              | {call, _, {atom, _, tuple}, []} %% Any Tuple
%%              | {type, _, tuple, []}            %% Empty Tuple
%%              | {type, _, tuple, FieldTypes}    %% Typed Tuple
%%              | {tuple, _, _}                   %% Tuple literal
%% @end
do_parse_type({type, _, tuple, any}, State) ->
    {{type, tuple, any}, State};
do_parse_type({call, _, {atom, _, tuple}, []}, State) ->
    {{type, tuple, any}, State};
do_parse_type({type, _, tuple, []}, State) ->
    {{type, tuple, empty}, State};
do_parse_type({type, _, tuple, FieldTypes0}, State0) ->
    {FieldTypes1, State1} = parse_tuple_fields(FieldTypes0, State0),
    {{type, tuple, FieldTypes1}, State1};
%%======================================
%% Union
%%======================================
%% @doc Expect :: {type, _, union, Types} %% Any Tuple
%% @end
do_parse_type({type, _, union, UnionTypes0}, State0) ->
    {UnionTypes1, State1} = parse_type_list(UnionTypes0, State0),
    {{type, union, UnionTypes1}, State1};
%%======================================
%% term()
%%======================================
%% @doc Expect :: {type, _, term, []}
%%              | {call, _, {atom, _, term}, []}
%%      Alias for any().
%%
%%      Calls are handled by the default call handler.
%% @end
do_parse_type({type, Line, term, []}, State) ->
    parse_type({type, Line, any, []}, State);
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
do_parse_type({type, Line, byte, []}, State) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 255}]},
               State);
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
do_parse_type({type, Line, char, []}, State) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 16#10ffff}]},
               State);
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
do_parse_type({type, Line, list, []}, State) ->
    parse_type({type, Line, list, [{type, Line, any, []}]}, State);
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
do_parse_type({type, Line, maybe_improper_list, []}, State) ->
    parse_type({type, Line, maybe_improper_list, [{type, Line, any, []}, {type, Line, any, []}]}, State);
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
do_parse_type({type, Line, nonempty_list, []}, State) ->
    parse_type({type, Line, nonempty_list, [{type, Line, any, []}]}, State);
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
do_parse_type({type, Line, string, []}, State) ->
    parse_type({type, Line, list, [{type, Line, char, []}]}, State);
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
do_parse_type({type, Line, nonempty_string, []}, State) ->
    parse_type({type, Line, nonempty_list, [{type, Line, char, []}]}, State);
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
do_parse_type({type, Line, iodata, []}, State) ->
    parse_type({type, Line, union, [{type, Line, iolist, []},
                                    {type, Line, binary, []}]},
               State);
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
do_parse_type({type, Line, iolist, []}, State) ->
    Form1 = {type, Line, union, [{type, Line, byte, []},
                                 {type, Line, binary, []},
                                 {type, iolist, []}]},
    Form2 = {type, Line, union, [{type, Line, binary, []},
                                 {type, Line, nil, []}]},
    parse_type({type, Line, maybe_improper_list, [Form1, Form2]}, State);
do_parse_type({type, iolist, []} = Iolist, State) ->
    {Iolist, State};
%%======================================
%% function()
%%======================================
%% @doc Expect :: {type, _, function, []}
%%              | {call, _, {atom, _, function}, []}
%%
%%      Alias for fun().
%% @end
do_parse_type({type, Line, function, []}, State) ->
    parse_type({type, Line, 'fun', []}, State);
do_parse_type({call, Line, {atom, _, function}, []}, State) ->
    parse_type({call, Line, {atom, Line, 'fun'}, []}, State);
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
do_parse_type({type, Line, module, []}, State) ->
    parse_type({type, Line, atom, []}, State);
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
do_parse_type({type, Line, mfa, []}, State) ->
    parse_type({type, Line, tuple, [{type, Line, module, []},
                                    {type, Line, atom, []},
                                    {type, Line, arity, []}]},
               State);
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
do_parse_type({type, Line, arity, []}, State) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {integer, Line, 255}]},
               State);
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
do_parse_type({type, Line, identifier, []}, State) ->
    parse_type({type, Line, union, [{type, Line, pid, []},
                                    {type, Line, port, []},
                                    {type, Line, reference, []}]},
               State);
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
do_parse_type({type, Line, node, []}, State) ->
    parse_type({type, Line, atom, []}, State);
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
do_parse_type({type, Line, timeout, []}, State) ->
    parse_type({type, Line, union, [{atom, Line, 'infinity'},
                                    {type, Line, non_neg_integer, []}]},
               State);
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
do_parse_type({type, Line, no_return, []}, State) ->
    parse_type({type, Line, none, []}, State);
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
do_parse_type({type, Line, non_neg_integer, []}, State) ->
    parse_type({type, Line, range, [{integer, Line, 0},
                                    {atom, Line, undefined}]},
               State);
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
do_parse_type({type, Line, pos_integer, []}, State) ->
    parse_type({type, Line, range, [{integer, Line, 1},
                                    {atom, Line, undefined}]},
               State);
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
do_parse_type({type, Line, neg_integer, []}, State) ->
    parse_type({type, Line, range, [{atom, Line, undefined},
                                    {op, Line, '-', {integer, Line, 1}}]},
               State);
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
do_parse_type({type, Line, nonempty_maybe_improper_list, []}, State) ->
    parse_type({type, Line, nonempty_maybe_improper_list, [{type, Line, any, []},
                                                           {type, Line, any, []}]},
               State);
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
do_parse_type({type, _, nonempty_maybe_improper_list, ValueTypes0}, State0) ->
    {ValueTypes1, State1} = parse_type_list(ValueTypes0, State0),
    {{type, list, list_to_tuple([nonempty | ValueTypes1])}, State1};
%%======================================
%% Record
%%======================================
%% @doc Expect :: {type, _, record, [Record | FieldOverrides]}
%%              | {record, _, Record, FieldOverrides}
%% @end
do_parse_type({type, _, record, [{atom, _, Record} | RecordFields0]}, State) ->
    RecordFields1 = lists:map(fun({type, _, field_type, [{atom, _, Field}, FieldType]}) ->
                                  {Field, parse_type(FieldType)}
                              end,
                              RecordFields0),
    {{type, record, {Record, RecordFields1}}, State};
%%======================================
%% Default call handler
%%======================================
do_parse_type({call, Line, {atom, _, Type}, TypeArgs}, State) ->
    parse_type({type, Line, Type, TypeArgs}, State);
%%======================================
%% Remote call handler
%%======================================
do_parse_type({call, Line, {remote, _, Module, Type}, TypeArgs}, State) ->
    parse_type({remote_type, Line, [Module, Type, TypeArgs]}, State);
%%======================================
%% Remote handler
%%======================================
do_parse_type({remote_type, _, RemoteTypeSpec} = TypeSpec, State0) ->
    [{atom, _, Module},
     {atom, _, Type},
     _] = RemoteTypeSpec,

    TypeKey = {Module, Type},
    ParsedKey = {parsed, Module},

    case State0 of
        #{TypeKey := RemoteType} ->
            {RemoteType, State0};
        #{ParsedKey := true} ->
            io:format("Could not find ~p\n~p\n", [TypeKey, State0]),
            {parse_type({type, 1, any, []}), State0};
        _ ->
            io:format("Parse Module ~p\n", [Module]),
            State1 = parse_types(Module, State0),
            io:format("Parsed Module ~p\n~p\n", [Module, State1]),
            State2 = maps:fold(fun(K, V, Acc) when is_atom(K) ->
                                      Acc#{{Module, K} => V};
                                  (_, _, Acc) ->
                                      Acc
                               end,
                               State0#{{parsed, Module} => true},
                               State1),
            parse_type(TypeSpec, State2)
    end;
%%======================================
%% Default handler
%%======================================
do_parse_type({Class, _, Type, TypeArgs}, Types0) when Class =:= type orelse
                                                    Class =:= user_type ->

    {TList, Types1} = parse_type_list(TypeArgs, Types0),
    {{type, Type, TList}, Types1};
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
    {literal, {atom, Line, undefined}};
parse_record_field_default({record_field, _, _, Default}) ->
    {literal, Default}.

%% @doc Extract the type from a record field.
%% @end
parse_record_field_type({typed_record_field, _, Type}, Types) ->
    parse_type(Type, Types);
parse_record_field_type(_, Types) ->
    parse_type({type, 1, any, []}, Types).

%%=============================================================================
%% state functions
%%=============================================================================
store_type(Label, Arity, Type, State) ->
    #state{module         = Module,
           types          = Types,
           parsed_modules = ParsedModules} = State,
    Key = {Module, Label, Arity},
    State#state{types          = Types#{Key => Type},
                parsed_modules = sets:add_element(Module, ParsedModules)}.

fetch_type(Label, Arity, State) ->
    fetch_type(State#state.module, Label, Arity, State).

fetch_type(Module, Label, Arity, State) ->
    Key = {Module, Label, Arity},
    #{Key := Type} = State#state.types,
    Type.










