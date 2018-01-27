-module(istype_parser_test).

-include_lib("eunit/include/eunit.hrl").

-define(TYPEANDCALL(Result, TypeAtom), fun() ->
                                           Type = istype_parser:parse_type({type, 1, TypeAtom, []}),
                                           Call = istype_parser:parse_type({call, 1, {atom, 1, TypeAtom}, []}),
                                           Type = Result,
                                           Call = Result
                                       end()).

%%=============================================================================
%% parse_types/1 Tests
%%=============================================================================

%%=============================================================================
%% parse_types/2 Tests
%%=============================================================================

%%=============================================================================
%% parse_type/1 Tests
%%=============================================================================
any_test() ->
    Result = {type, any, []},
    ?TYPEANDCALL(Result, any).

none_test() ->
    Result = {type, none, []},
    ?TYPEANDCALL(Result, none).

pid_test() ->
    Result = {type, pid, []},
    ?TYPEANDCALL(Result, pid).

port_test() ->
    Result = {type, port, []},
    ?TYPEANDCALL(Result, port).

reference_test() ->
    Result = {type, reference, []},
    ?TYPEANDCALL(Result, reference).

nil_test() ->
    Result = {literal, {nil, 1}},
    Result = istype_parser:parse_type({nil, 1}),
    ?TYPEANDCALL(Result, nil).

atom_test() ->
    Result = {type, atom, []},
    ?TYPEANDCALL(Result, atom).

'ErlangAtom_test'() ->
    {literal, {atom, 1, 'ErlangAtom'}} = istype_parser:parse_type({atom, 1, 'ErlangAtom'}).

'Bitstring_test'() ->
    {type, bitstring, {1, 2}} = istype_parser:parse_type({type, 1, binary, [{integer, 1, 1},
                                                                            {integer, 1, 2}]}).

'Fun_test'() ->
    Result1 = {type, 'fun', any},
    ?TYPEANDCALL(Result1, 'fun'),

    Result2 = {type, 'fun', {any, {type, atom, []}}},
    Result2 = istype_parser:parse_type({type, 1, 'fun', [{type, 1, any}, {type, 1, atom, []}]}),

    Result3 = {type, 'fun', {[], {type, atom, []}}},
    Result3 = istype_parser:parse_type({type, 1, 'fun', [{type, 1, product, []}, {type, 1, atom, []}]}),

    Result4 = {type, 'fun', {[{type, atom, []}], {type, atom, []}}},
    Result4 = istype_parser:parse_type({type, 1, 'fun', [{type, 1, product, [{type, 1, atom, []}]}, {type, 1, atom, []}]}).

float_test() ->
    Result = {type, float, []},
    ?TYPEANDCALL(Result, float).

integer_test() ->
    Result = {type, integer, []},
    ?TYPEANDCALL(Result, integer).

'Integer_test'() ->
    {literal, {integer, 1, 1}} = istype_parser:parse_type({integer, 1, 1}),
    {literal, {op, 1, '-', {integer, 1, 1}}} = istype_parser:parse_type({op, 1, '-', {integer, 1, 1}}).

'Integer..Integer_test'() ->
    Result = {type, range, {{literal, {integer, 1, 1}},
                            {literal, {integer, 1, 2}}}},
    Result = istype_parser:parse_type({type, 1, range, [{integer, 1, 1},
                                                           {integer, 1, 2}]}).

'List_test'() ->
    {type, list, any} = istype_parser:parse_type({type, 1, list, [{type, 1, any, []}]}),
    {type, list, any} = istype_parser:parse_type({call, 1, {atom, 1, list}, [{type, 1, any, []}]}),

    {type, list, {maybe_empty, {type, integer, []}, {literal, {nil, 1}}}} = istype_parser:parse_type({type, 1, list, [{type, 1, integer, []}]}),
    {type, list, {maybe_empty, {type, integer, []}, {literal, {nil, 1}}}} = istype_parser:parse_type({call, 1, {atom, 1, list}, [{call, 1, {atom, 1, integer}, []}]}),

    {type, list, {maybe_empty, {type, integer, []}, {type, any, []}}} = istype_parser:parse_type({type, 1, maybe_improper_list, [{type, 1, integer, []}, {type, 1, any, []}]}),
    {type, list, {maybe_empty, {type, integer, []}, {type, any, []}}} = istype_parser:parse_type({call, 1, {atom, 1, maybe_improper_list}, [{call, 1, {atom, 1, integer}, []},
                                                                                                                                            {call, 1, {atom, 1, any}, []}]}),

    {type, list, {nonempty, {type, integer, []}, {type, any, []}}} = istype_parser:parse_type({type, 1, nonempty_improper_list, [{type, 1, integer, []}, {type, 1, any, []}]}),
    {type, list, {nonempty, {type, integer, []}, {type, any, []}}} = istype_parser:parse_type({call, 1, {atom, 1, nonempty_improper_list}, [{call, 1, {atom, 1, integer}, []},
                                                                                                                                            {call, 1, {atom, 1, any}, []}]}),

    {type, list, {nonempty, {type, any, []}, {literal, {nil, 1}}}} = istype_parser:parse_type({type, 1, nonempty_list, [{type, 1, any, []}]}),
    {type, list, {nonempty, {type, any, []}, {literal, {nil, 1}}}} = istype_parser:parse_type({call, 1, {atom, 1, nonempty_list}, [{type, 1, any, []}]}).

'Map_test'() ->
    {type, map, any} = istype_parser:parse_type({type, 1, map, any}),
    {type, map, any} = istype_parser:parse_type({call, 1, {atom, 1, map}, []}),

    {type, map, empty} = istype_parser:parse_type({type, 1, map, []}),

    {type, map, {[{{type, atom, []},
                   {type, atom, []}}],
                 []}} = istype_parser:parse_type({type, 1, map, [{type, 1, map_field_exact, [{type, 1, atom, []}, {type, 1, atom, []}]}]}),

    {type, map, {[],
                 [{{type, binary, []},
                   {type, binary, []}}]}} = istype_parser:parse_type({type, 1, map, [{type, 1, map_field_assoc, [{type, 1, binary, []}, {type, 1, binary, []}]}]}),

    {type, map, {[{{type, atom, []},
                   {type, atom, []}}],
                 [{{type, binary, []},
                   {type, binary, []}}]}} = istype_parser:parse_type({type, 1, map, [{type, 1, map_field_exact, [{type, 1, atom, []}, {type, 1, atom, []}]},
                                                                                        {type, 1, map_field_assoc, [{type, 1, binary, []}, {type, 1, binary, []}]}]}).

'Tuple_test'() ->
    {type, tuple, any} = istype_parser:parse_type({type, 1, tuple, any}),
    {type, tuple, any} = istype_parser:parse_type({call, 1, {atom, 1, tuple}, []}),

    {type, tuple, empty} = istype_parser:parse_type({type, 1, tuple, []}),
    {type, tuple, {1, [{1, {type, integer, []}}]}} = istype_parser:parse_type({type, 1, tuple, [{type, 1, integer, []}]}),
    {type, tuple, {2, [{1, {type, integer, []}}, {2, {type, atom, []}}]}} = istype_parser:parse_type({type, 1, tuple, [{type, 1, integer, []}, {type, 1, atom, []}]}),
    {type, tuple, {2, [{1, {type, integer, []}}, {2, {type, any, []}}]}} = istype_parser:parse_type({type, 1, tuple, [{type, 1, integer, []}, {var, 1, '_'}]}).

'Union_test'() ->
    {type, union, [{type, integer, []}]} = istype_parser:parse_type({type, 1, union, [{type, 1, integer, []}]}),
    {type, union, [{type, integer, []}, {type, atom, []}]} = istype_parser:parse_type({type, 1, union, [{type, 1, integer, []}, {type, 1, atom, []}]}).

term_test() ->
    Result = {type, any, []},
    ?TYPEANDCALL(Result, term).

binary_test() ->
    Result = {type, binary, []},
    ?TYPEANDCALL(Result, binary).

bitstring_test() ->
    Result = {type, bitstring, []},
    ?TYPEANDCALL(Result, bitstring).

boolean_test() ->
    Result = {type, boolean, []},
    ?TYPEANDCALL(Result, boolean).

byte_test() ->
    Result = {type, range, {{literal, {integer, 1, 0}}, {literal, {integer, 1, 255}}}},
    ?TYPEANDCALL(Result, byte).

char_test() ->
    Result = {type, range, {{literal, {integer, 1, 0}}, {literal, {integer, 1, 16#10ffff}}}},
    ?TYPEANDCALL(Result, char).

number_test() ->
    Result = {type, number, []},
    ?TYPEANDCALL(Result, number).

list_test() ->
    Result = {type, list, any},
    ?TYPEANDCALL(Result, list).

maybe_improper_list_test() ->
    Result = {type, list, {maybe_empty, {type, any, []}, {type, any, []}}},
    ?TYPEANDCALL(Result, maybe_improper_list).

nonempty_list_test() ->
    Result = {type, list, {nonempty, {type, any, []}, {literal, {nil, 1}}}},
    ?TYPEANDCALL(Result, nonempty_list).

string_test() ->
    Result = {type, list, {maybe_empty, {type, range, {{literal, {integer, 1, 0}}, {literal, {integer, 1, 16#10ffff}}}}, {literal, {nil, 1}}}},
    ?TYPEANDCALL(Result, string).

nonempty_string_test() ->
    Result = {type, list, {nonempty, {type, range, {{literal, {integer, 1, 0}}, {literal, {integer, 1, 16#10ffff}}}}, {literal, {nil, 1}}}},
    ?TYPEANDCALL(Result, nonempty_string).

iodata_test() ->
    Result = {type, union, [{type, list, {maybe_empty,
                                          {type, union, [{type, range, {{literal, {integer, 1, 0}},
                                                                        {literal, {integer, 1, 255}}}},
                                                         {type, binary, []},
                                                         {type, iolist, []}]},
                                          {type, union, [{type, binary, []},
                                                         {literal, {nil, 1}}]}}},
                            {type, binary, []}]},
    ?TYPEANDCALL(Result, iodata).

iolist_test() ->
    Result = {type, list, {maybe_empty,
                           {type, union, [{type, range, {{literal, {integer, 1, 0}},
                                                         {literal, {integer, 1, 255}}}},
                                          {type, binary, []},
                                          {type, iolist, []}]},
                           {type, union, [{type, binary, []},
                                          {literal, {nil, 1}}]}}},
    ?TYPEANDCALL(Result, iolist).

function_test() ->
    Result = {type, 'fun', any},
    ?TYPEANDCALL(Result, function).

module_test() ->
    Result = {type, atom, []},
    ?TYPEANDCALL(Result, module).

mfa_test() ->
    Result = {type, tuple, {3, [{1, {type, atom, []}},
                                {2, {type, atom, []}},
                                {3, {type, range, {{literal, {integer, 1, 0}},
                                                   {literal, {integer, 1, 255}}}}}]}},
    ?TYPEANDCALL(Result, mfa).

arity_test() ->
    Result = {type, range, {{literal, {integer, 1, 0}},
                            {literal, {integer, 1, 255}}}},
    ?TYPEANDCALL(Result, arity).

identifier_test() ->
    Result = {type, union, [{type, pid, []},
                            {type, port, []},
                            {type, reference, []}]},
    ?TYPEANDCALL(Result, identifier).

node_test() ->
    Result = {type, atom, []},
    ?TYPEANDCALL(Result, node).

timeout_test() ->
    Result = {type, union, [{literal, {atom, 1, infinity}},
                            {type, range, {{literal, {integer, 1, 0}},
                                           {literal, {atom, 1, undefined}}}}]},
    ?TYPEANDCALL(Result, timeout).

no_return_test() ->
    Result = {type, none, []},
    ?TYPEANDCALL(Result, no_return).

non_neg_integer_test() ->
    Result = {type, range, {{literal, {integer, 1, 0}},
                            {literal, {atom, 1, undefined}}}},
    ?TYPEANDCALL(Result, non_neg_integer).

pos_integer_test() ->
    Result = {type, range, {{literal, {integer, 1, 1}},
                            {literal, {atom, 1, undefined}}}},
    ?TYPEANDCALL(Result, pos_integer).

neg_integer_test() ->
    Result = {type, range, {{literal, {atom, 1, undefined}},
                            {literal, {op, 1, '-', {integer, 1, 1}}}}},
    ?TYPEANDCALL(Result, neg_integer).

type_record_test() ->
    Result1 = {type, record, {recorda, []}},
    Result1 = istype_parser:parse_type({type, 1, record, [{atom, 1, recorda}]}),

    Result2 = {type, record, {recorda, [{a, {type, float, []}}]}},
    Result2 = istype_parser:parse_type({type, 1, record, [{atom, 1, recorda},
                                                          {type, 7, field_type, [{atom, 1, a}, {type, 1, float, []}]}]}).
remote_test() ->
    Result = {type, atom, []},
    Result = istype_parser:parse_type({remote_type, 1, [{atom, 1, istype_types_test}, {atom, 1, typea}, []]}),
    Result = istype_parser:parse_type({call, 1, {remote, 1, {atom, 1, istype_types_test}, {atom, 1, typea}}, []}).

%%=============================================================================
%% parse_record/1 Tests
%%=============================================================================
record_test() ->
    Result1 = {record, recorda, 2, [a], #{a => {type, any, []}}, #{a => {literal, {atom, 1, undefined}}}},
    Result1 = istype_parser:parse_record({attribute, 1, record, {recorda, [{record_field, 1, {atom, 1, a}}]}}),

    Result2 = {record, recorda, 2, [a], #{a => {type, atom, []}}, #{a => {literal, {atom, 1, undefined}}}},
    Result2 = istype_parser:parse_record({attribute, 1, record, {recorda, [{typed_record_field, {record_field, 1, {atom, 1, a}}, {type, 1, atom, []}}]}}),

    Result3 = {record, recorda, 2, [a], #{a => {type, atom, []}}, #{a => {literal, {atom, 1, atom}}}},
    Result3 = istype_parser:parse_record({attribute, 1, record, {recorda, [{typed_record_field, {record_field, 1, {atom, 1, a}, {atom, 1, atom}}, {type, 1, atom, []}}]}}).
