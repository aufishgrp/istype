-module(istype_parser_test).

-include("istype.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TYPEANDCALL(TypeAtom), ?TYPEANDCALL(#type{type = TypeAtom}, TypeAtom)).

-define(TYPEANDCALL(Result, TypeAtom), fun() ->
                                           Type = istype_parser:parse_type({type, 1, TypeAtom, []}),
                                           Call = istype_parser:parse_type({call, 1, {atom, 1, TypeAtom}, []}),
                                           ?assertEqual(Result, Type),
                                           ?assertEqual(Result, Call)
                                       end()).

%%=============================================================================
%% parse_types/1 Tests
%%=============================================================================
parse1_test() ->
    Forms0 = [{attribute, 1, module, ?MODULE}],
    Result0 = #{},
    ?assertEqual(Result0, istype_parser:parse_types(Forms0)),
    
    Forms1 = [{attribute, 1, module, ?MODULE},
              {attribute, 1, type, {typea, {type, 1, atom, []}, []}}],
    Result1 = #{{?MODULE, typea, 0} => #type{module = ?MODULE, type = atom}},
    ?assertEqual(Result1, istype_parser:parse_types(Forms1)).

%%=============================================================================
%% parse_types/2 Tests
%%=============================================================================
parse2_test() ->
    Forms0 = [{attribute, 1, module, ?MODULE},
              {attribute, 1, type, {typea, {type, 1, atom, []}, []}}],
    Result0 = #{{?MODULE, typea, 0} => #type{module = ?MODULE, type = atom}},
    ?assertEqual(Result0, istype_parser:parse_types(Forms0, #{})),

    Forms1 = [{attribute, 1, module, ?MODULE},
              {attribute, 1, type, {typeb, {type, 1, binary, []}, []}}],
    Result1 = Result0#{{?MODULE, typeb, 0} => #type{module = ?MODULE, type = binary}},
    ?assertEqual(Result1, istype_parser:parse_types(Forms1, Result0)).

%%=============================================================================
%% parse_type/1 Tests
%%=============================================================================
any_test() ->
    ?TYPEANDCALL(any).

none_test() ->
    ?TYPEANDCALL(none).

pid_test() ->
    ?TYPEANDCALL(pid).

port_test() ->
    ?TYPEANDCALL(port).

reference_test() ->
    ?TYPEANDCALL(reference).

nil_test() ->
    Result = #literal{value = {nil, 1}},
    Result = istype_parser:parse_type({nil, 1}),
    ?TYPEANDCALL(Result, nil).

atom_test() ->
    ?TYPEANDCALL(atom).

'ErlangAtom_test'() ->
    ?assertEqual(#literal{value = {atom, 1, 'ErlangAtom'}},
                 istype_parser:parse_type({atom, 1, 'ErlangAtom'})).

'Bitstring_test'() ->
    ?assertEqual(#type{type = bitstring,
                       spec = {1, 2}},
                 istype_parser:parse_type({type, 1, binary, [{integer, 1, 1},
                                                             {integer, 1, 2}]})).

'Fun_test'() ->
    Result0 = #type{type = 'fun',
                    spec = any},
    ?TYPEANDCALL(Result0, 'fun'),

    Result1 = #type{type = 'fun',
                    spec = {any, #type{type = atom}}},
    ?assertEqual(Result1, istype_parser:parse_type({type, 1, 'fun', [{type, 1, any}, {type, 1, atom, []}]})),

    Result2 = #type{type = 'fun',
                    spec = {[], #type{type = atom}}},
    ?assertEqual(Result2, istype_parser:parse_type({type, 1, 'fun', [{type, 1, product, []}, {type, 1, atom, []}]})),

    Result3 = #type{type = 'fun',
                    spec = {[#type{type = atom}],
                             #type{type = atom}}},
    ?assertEqual(Result3, istype_parser:parse_type({type, 1, 'fun', [{type, 1, product, [{type, 1, atom, []}]}, {type, 1, atom, []}]})).

float_test() ->
    ?TYPEANDCALL(float).

integer_test() ->
    ?TYPEANDCALL(integer).

'Integer_test'() ->
    ?assertEqual(#literal{value = {integer, 1, 1}},  istype_parser:parse_type({integer, 1, 1})),
    ?assertEqual(#literal{value = {op, 1, '-', {integer, 1, 1}}}, istype_parser:parse_type({op, 1, '-', {integer, 1, 1}})).

'Integer..Integer_test'() ->
    Result = #type{type = range,
                   spec = {#literal{value = {integer, 1, 1}},
                                #literal{value = {integer, 1, 2}}}},
    ?assertEqual(Result, istype_parser:parse_type({type, 1, range, [{integer, 1, 1},
                                                                    {integer, 1, 2}]})).

'List_test'() ->
    ?assertEqual(#type{type = list, spec = any}, istype_parser:parse_type({type, 1, list, [{type, 1, any, []}]})),
    ?assertEqual(#type{type = list, spec = any}, istype_parser:parse_type({call, 1, {atom, 1, list}, [{type, 1, any, []}]})),

    ?assertEqual(#type{type = list,
                       spec = {maybe_empty,
                                    #type{type = integer},
                                    #literal{value = {nil, 1}}}},
                 istype_parser:parse_type({type, 1, list, [{type, 1, integer, []}]})),
    ?assertEqual(#type{type = list,
                       spec = {maybe_empty,
                                    #type{type = integer},
                                    #literal{value = {nil, 1}}}},
                 istype_parser:parse_type({call, 1, {atom, 1, list}, [{call, 1, {atom, 1, integer}, []}]})),

    ?assertEqual(#type{type = list,
                       spec = {maybe_empty,
                                    #type{type = integer},
                                    #type{type = any}}},
                 istype_parser:parse_type({type, 1, maybe_improper_list, [{type, 1, integer, []}, {type, 1, any, []}]})),
    ?assertEqual(#type{type = list,
                       spec = {maybe_empty,
                                    #type{type = integer},
                                    #type{type = any}}},
                 istype_parser:parse_type({call, 1, {atom, 1, maybe_improper_list}, [{call, 1, {atom, 1, integer}, []},
                                          {call, 1, {atom, 1, any}, []}]})),

    ?assertEqual(#type{type = list,
                       spec = {nonempty,
                                    #type{type = integer},
                                    #type{type = any}}},
                 istype_parser:parse_type({type, 1, nonempty_improper_list, [{type, 1, integer, []}, {type, 1, any, []}]})),
    ?assertEqual(#type{type = list,
                       spec = {nonempty,
                                    #type{type = integer},
                                    #type{type = any}}},
                 istype_parser:parse_type({call, 1, {atom, 1, nonempty_improper_list}, [{call, 1, {atom, 1, integer}, []},
                                          {call, 1, {atom, 1, any}, []}]})),

    ?assertEqual(#type{type = list,
                       spec = {nonempty,
                                    #type{type = any},
                                    #literal{value = {nil, 1}}}},
                 istype_parser:parse_type({type, 1, nonempty_list, [{type, 1, any, []}]})),
    ?assertEqual(#type{type = list,
                       spec = {nonempty,
                                    #type{type = any},
                                    #literal{value = {nil, 1}}}},
                 istype_parser:parse_type({call, 1, {atom, 1, nonempty_list}, [{type, 1, any, []}]})).

'Map_test'() ->
    ?assertEqual(#type{type = map, spec = any}, istype_parser:parse_type({type, 1, map, any})),
    ?assertEqual(#type{type = map, spec = any}, istype_parser:parse_type({call, 1, {atom, 1, map}, []})),

    ?assertEqual(#type{type = map, spec = empty}, istype_parser:parse_type({type, 1, map, []})),

    ?assertEqual(#type{type = map,
                       spec = {[{#type{type = atom}, #type{type = atom}}],
                               []}},
                 istype_parser:parse_type({type, 1, map, [{type, 1, map_field_exact, [{type, 1, atom, []}, {type, 1, atom, []}]}]})),

    ?assertEqual(#type{type = map,
                       spec = {[],
                               [{#type{type = binary}, #type{type = binary}}]}},
                 istype_parser:parse_type({type, 1, map, [{type, 1, map_field_assoc, [{type, 1, binary, []}, {type, 1, binary, []}]}]})),

    ?assertEqual(#type{type = map,
                       spec = {[{#type{type = atom}, #type{type = atom}}],
                               [{#type{type = binary}, #type{type = binary}}]}},
                 istype_parser:parse_type({type, 1, map, [{type, 1, map_field_exact, [{type, 1, atom, []}, {type, 1, atom, []}]},
                                                          {type, 1, map_field_assoc, [{type, 1, binary, []}, {type, 1, binary, []}]}]})).

'Tuple_test'() ->
    ?assertEqual(#type{type = tuple, spec = any}, istype_parser:parse_type({type, 1, tuple, any})),
    ?assertEqual(#type{type = tuple, spec = any}, istype_parser:parse_type({call, 1, {atom, 1, tuple}, []})),

    ?assertEqual(#type{type = tuple, spec = empty}, istype_parser:parse_type({type, 1, tuple, []})),

    ?assertEqual(#type{type = tuple,
                       spec = {1, [{1, #type{type = integer}}]}},
                 istype_parser:parse_type({type, 1, tuple, [{type, 1, integer, []}]})),
    ?assertEqual(#type{type = tuple,
                       spec = {2, [{1, #type{type = integer}},
                                   {2, #type{type = atom}}]}},
                 istype_parser:parse_type({type, 1, tuple, [{type, 1, integer, []}, {type, 1, atom, []}]})),
    ?assertEqual(#type{type = tuple,
                       spec = {2, [{1, #type{type = integer}},
                                   {2, #type{type = any}}]}},
                 istype_parser:parse_type({type, 1, tuple, [{type, 1, integer, []}, {var, 1, '_'}]})).

'Union_test'() ->
    ?assertEqual(#type{type = union,
                       spec = [#type{type = integer}]},
                 istype_parser:parse_type({type, 1, union, [{type, 1, integer, []}]})),
    ?assertEqual(#type{type = union,
                       spec = [#type{type = integer},
                               #type{type = atom}]},
                 istype_parser:parse_type({type, 1, union, [{type, 1, integer, []}, {type, 1, atom, []}]})).

term_test() ->
    Result = #type{type = any},
    ?TYPEANDCALL(Result, term).

binary_test() ->
    ?TYPEANDCALL(binary).

bitstring_test() ->
    ?TYPEANDCALL(bitstring).

boolean_test() ->
    ?TYPEANDCALL(boolean).

byte_test() ->
    Result = #type{type = range,
                   spec = {#literal{value = {integer, 1, 0}},
                           #literal{value = {integer, 1, 255}}}},
    ?TYPEANDCALL(Result, byte).

char_test() ->
    Result = #type{type = range,
                   spec = {#literal{value = {integer, 1, 0}},
                           #literal{value = {integer, 1, 16#10ffff}}}},
    ?TYPEANDCALL(Result, char).

number_test() ->
    ?TYPEANDCALL(number).

list_test() ->
    Result = #type{type = list, spec = any},
    ?TYPEANDCALL(Result, list).

maybe_improper_list_test() ->
    Result = #type{type = list,
                   spec = {maybe_empty,
                                #type{type = any},
                                #type{type = any}}},
    ?TYPEANDCALL(Result, maybe_improper_list).

nonempty_list_test() ->
    Result = #type{type = list,
                   spec = {nonempty,
                                #type{type = any},
                                #literal{value = {nil, 1}}}},
    ?TYPEANDCALL(Result, nonempty_list).

string_test() ->
    Result = #type{type = list,
                   spec = {maybe_empty,
                           #type{type = range,
                                 spec = {#literal{value = {integer, 1, 0}},
                                         #literal{value = {integer, 1, 16#10ffff}}}},
                           #literal{value = {nil, 1}}}},
    ?TYPEANDCALL(Result, string).

nonempty_string_test() ->
    Result = #type{type = list,
                   spec = {nonempty,
                           #type{type = range,
                                 spec = {#literal{value = {integer, 1, 0}},
                                         #literal{value = {integer, 1, 16#10ffff}}}},
                           #literal{value = {nil, 1}}}},
    ?TYPEANDCALL(Result, nonempty_string).

iodata_test() ->
    Result = #type{type = union,
                   spec = [#type{type = list,
                                 spec = {maybe_empty,
                                         #type{type = union,
                                                spec = [#type{type = range,
                                                              spec = {#literal{value = {integer, 1, 0}},
                                                                      #literal{value = {integer, 1, 255}}}},
                                                        #type{type = binary},
                                                        #type{type = iolist}]},
                                          #type{type = union,
                                                spec = [#type{type = binary},
                                                        #literal{value = {nil, 1}}]}}},
                           #type{type = binary}]},
    ?TYPEANDCALL(Result, iodata).

iolist_test() ->
    Result = #type{type = list,
                   spec = {maybe_empty,
                           #type{type = union,
                                  spec = [#type{type = range,
                                                spec = {#literal{value = {integer, 1, 0}},
                                                        #literal{value = {integer, 1, 255}}}},
                                          #type{type = binary},
                                          #type{type = iolist}]},
                            #type{type = union,
                                  spec = [#type{type = binary},
                                          #literal{value = {nil, 1}}]}}},
    ?TYPEANDCALL(Result, iolist).

function_test() ->
    Result = #type{type = 'fun', spec = any},
    ?TYPEANDCALL(Result, function).

module_test() ->
    Result = #type{type = atom},
    ?TYPEANDCALL(Result, module).

mfa_test() ->
    Result = #type{type = tuple,
                   spec = {3, [{1, #type{type = atom}},
                               {2, #type{type = atom}},
                               {3, #type{type = range,
                                         spec = {#literal{value = {integer, 1, 0}},
                                                 #literal{value = {integer, 1, 255}}}}}]}},
    ?TYPEANDCALL(Result, mfa).

arity_test() ->
    Result = #type{type = range,
                   spec = {#literal{value = {integer, 1, 0}},
                           #literal{value = {integer, 1, 255}}}},
    ?TYPEANDCALL(Result, arity).

identifier_test() ->
    Result = #type{type = union,
                   spec = [#type{type = pid},
                           #type{type = port},
                           #type{type = reference}]},
    ?TYPEANDCALL(Result, identifier).

node_test() ->
    Result = #type{type = atom},
    ?TYPEANDCALL(Result, node).

timeout_test() ->
    Result = #type{type = union,
                   spec = [#literal{value = {atom, 1, infinity}},
                           #type{type = range,
                                 spec = {#literal{value = {integer, 1, 0}},
                                         #literal{value = {atom, 1, undefined}}}}]},
    ?TYPEANDCALL(Result, timeout).

no_return_test() ->
    Result = #type{type = none},
    ?TYPEANDCALL(Result, no_return).

non_neg_integer_test() ->
    Result = #type{type = range,
                   spec = {#literal{value = {integer, 1, 0}},
                           #literal{value = {atom, 1, undefined}}}},
    ?TYPEANDCALL(Result, non_neg_integer).

pos_integer_test() ->
    Result = #type{type = range,
                   spec = {#literal{value = {integer, 1, 1}},
                           #literal{value = {atom, 1, undefined}}}},
    ?TYPEANDCALL(Result, pos_integer).

neg_integer_test() ->
    Result = #type{type = range,
                   spec = {#literal{value = {atom, 1, undefined}},
                           #literal{value = {op, 1, '-', {integer, 1, 1}}}}},
    ?TYPEANDCALL(Result, neg_integer).

type_record_test() ->
    Result0 = #type{type = record,
                    spec = {record_a, []}},
    ?assertEqual(Result0, istype_parser:parse_type({type, 1, record, [{atom, 1, record_a}]})),

    Result1 = #type{type = record,
                    spec = {record_a, [{a, #type{type = float}}]}},
    ?assertEqual(Result1, istype_parser:parse_type({type, 1, record, [{atom, 1, record_a},
                                                                      {type, 7, field_type, [{atom, 1, a}, {type, 1, float, []}]}]})).
remote_test() ->
    Result = #type{module = istype_remote, type = atom},
    ?assertEqual(Result, istype_parser:parse_type({remote_type, 1, [{atom, 1, istype_remote}, {atom, 1, type_a}, []]})),
    ?assertEqual(Result, istype_parser:parse_type({call, 1, {remote, 1, {atom, 1, istype_remote}, {atom, 1, type_a}}, []})).

paramaterized_test() ->
    Result = #type{type   = tuple,
                   spec   = {1, [{1, {var, 1, 'A'}}]},
                   params = [{var, 1, 'A'}]},
    ?assertEqual(Result, istype_parser:parse_type({attribute, 1, type, {type_a, {type, 1, tuple, [{var, 1, 'A'}]}, [{var, 1, 'A'}]}})).

resolve_type_test() ->
    %% type_0(A, B) :: {A, B}.
    %% type_1(A)    :: type_0(A, binary()).
    %% type_2()     :: type_1(atom()).
    Type0 = #type{type   = tuple,
                  spec   = {2, [{1, {var, 1, 'A'}},
                                {2, {var, 1, 'B'}}]},
                  params = [{var, 1, 'A'},
                            {var, 1, 'B'}]},
    Type1 = #type{type   = type_0,
                  spec   = [{var, 1, 'A'},
                            #type{type = binary}],
                  params = [{var, 1, 'A'}]},
    Type2 = #type{type   = type_1,
                  spec   = [#type{type = atom}]},
    Types = #{{undefined, type_0, 2} => Type0,
              {undefined, type_1, 1} => Type1,
              {undefined, type_2, 0} => Type2},

    Result0 = #type{type = tuple,
                    spec = {2, [{1, #type{type = atom}},
                                {2, #type{type = binary}}]}},
    ?assertEqual(Result0, istype_parser:resolve_type(#type{type = type_2},
                                                     Types)),

    Result1 = #type{type = tuple,
                    spec = {2, [{1, #type{type = binary}},
                                {2, #type{type = binary}}]}},
    ?assertEqual(Result1, istype_parser:resolve_type(#type{type = type_1,
                                                           spec = [#type{type = binary}]},
                                                     Types)),

    Result2 = #type{type = tuple,
                    spec = {2, [{1, #type{type = atom}},
                                {2, #type{type = atom}}]}},
    ?assertEqual(Result2, istype_parser:resolve_type(#type{type = type_0,
                                                           spec = [#type{type = atom},
                                                                   #type{type = atom}]},
                                                     Types)).

%%=============================================================================
%% parse_record/1 Tests
%%=============================================================================
record_test() ->
    Result1 = {record, record_a, 2, [a], #{a => #type{type = any}}, #{a => #literal{value = {atom, 1, undefined}}}},
    Result1 = istype_parser:parse_record({attribute, 1, record, {record_a, [{record_field, 1, {atom, 1, a}}]}}),

    Result2 = {record, record_a, 2, [a], #{a => #type{type = atom}}, #{a => #literal{value = {atom, 1, undefined}}}},
    Result2 = istype_parser:parse_record({attribute, 1, record, {record_a, [{typed_record_field, {record_field, 1, {atom, 1, a}}, {type, 1, atom, []}}]}}),

    Result3 = {record, record_a, 2, [a], #{a => #type{type = atom}}, #{a => #literal{value = {atom, 1, atom}}}},
    Result3 = istype_parser:parse_record({attribute, 1, record, {record_a, [{typed_record_field, {record_field, 1, {atom, 1, a}, {atom, 1, atom}}, {type, 1, atom, []}}]}}).
