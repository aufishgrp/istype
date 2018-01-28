-module(istype_validator_test).

-include_lib("eunit/include/eunit.hrl").

-define(BIF(Type, Bif), fun() ->
                            Value = {atom, 1, atom},
                            Expected = {call, 1, {atom, 1, Bif}, [Value]},
                            istype_test_util:match(istype_validator, Value, {type, Type, []}, Expected, [])
                        end()).

-define(REMOTE(Value, Type), fun() ->
                                 Abs = erl_parse:abstract(Type, [{line, 1}]),
                                 Expected = {call, 1,
                                                {remote, 1, {atom, 1, istype_lib}, {atom, 1, transform}},
                                                 [Value,
                                                  Abs,
                                                  {map, 1, []},
                                                  {map, 1, []},
                                                  {nil, 1}]},
                                 istype_test_util:match(istype_validator, Value, Type, Expected, [])
                             end()).

literal_test() ->
    Value = {atom, 1, atom},
    Literal = {atom, 1, also},
    Type = {literal, Literal},
    Expected = {op, 1, '=:=', Value, Literal},

    istype_test_util:match(istype_validator, Value, Type, Expected, []).

any_test() ->
    istype_test_util:match(istype_validator, {atom, 1, atom}, {type, any, []}, {atom, 1, true}, []).

none_test() ->
    istype_test_util:match(istype_validator, {atom, 1, atom}, {type, none, []}, {atom, 1, false}, []).

pid_test() ->
    ?BIF(pid, is_pid).

port_test() ->
    ?BIF(port, is_port).

reference_test() ->
    ?BIF(reference, is_reference).

atom_test() ->
    ?BIF(atom, is_atom).

'Bitstring_test'() ->
    Value = {atom, 1, atom},

    M0 = 0,
    N0 = 0,
    Type0 = {type, bitstring, {M0, N0}},
    Expected0 = {op, 1, '=:=', Value, {bin, 1, []}},
    istype_test_util:match(istype_validator, Value, Type0, Expected0, []),
    
    M1 = 7,
    N1 = 0,
    Type1 = {type, bitstring, {M1, N1}},
    Expected1 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_bitstring}, [Value]},
                    {op, 1, '=:=',
                        {call, 1, {atom, 1, bit_size}, [Value]},
                        {integer, 1, M1}}},
    istype_test_util:match(istype_validator, Value, Type1, Expected1, []),

    M2 = 0,
    N2 = 7,
    Type2 = {type, bitstring, {M2, N2}},
    Expected2 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_bitstring}, [Value]},
                    {op, 1, '=:=',
                        {op, 1, 'rem',
                            {call, 1, {atom, 1, bit_size}, [Value]},
                            {integer, 1, N2}},
                        {integer, 1, M2}}},
    istype_test_util:match(istype_validator, Value, Type2, Expected2, []),                        

    M3 = 7,
    N3 = 7,
    Type3 = {type, bitstring, {M3, N3}},
    Expected3 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_bitstring}, [Value]},
                    {op, 1, '=:=',
                        {op, 1, 'rem',
                            {call, 1, {atom, 1, bit_size}, [Value]},
                            {integer, 1, N3}},
                        {integer, 1, M3}}},
    istype_test_util:match(istype_validator, Value, Type3, Expected3, []).

float_test() ->
    ?BIF(float, is_float).

integer_test() ->
    ?BIF(integer, is_integer).

'Integer..Integer_test'() ->
    Value = {atom, 1, atom},

    Type0 = {type, range, {{literal, {integer, 1, 1}},
                           {literal, {integer, 1, 2}}}},
    Expected0 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_integer}, [Value]},
                    {op, 1, 'andalso',
                        {op, 1, '>=', Value, {integer, 1, 1}},
                        {op, 1, '=<', Value, {integer, 1, 2}}}},
    istype_test_util:match(istype_validator, Value, Type0, Expected0, []),

    Type1 = {type, range, {{literal, {integer, 1, 1}}, {literal, {atom, 1, undefined}}}},
    Expected1 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_integer}, [Value]},
                    {op, 1, '>=', Value, {integer, 1, 1}}},
    istype_test_util:match(istype_validator, Value, Type1, Expected1, []),

    Type2 = {type, range, {{literal, {atom, 1, undefined}}, {literal, {integer, 1, 2}}}},
    Expected2 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_integer}, [Value]},
                    {op, 1, '=<', Value, {integer, 1, 2}}},
    istype_test_util:match(istype_validator, Value, Type2, Expected2, []).

'List_test'() ->
    Value = {atom, 1, atom},

    Type0 = {type, list, {maybe_empty, {type, any, []}, {type, any, []}}},
    Expected0 = {call, 1, {atom, 1, is_list}, [Value]},
    istype_test_util:match(istype_validator, Value, Type0, Expected0, []),

    Type1 = {type, list, {maybe_empty, {type, any, []}, {literal, {nil, 1}}}},
    Expected1 = {call, 1, {atom, 1, is_list}, [Value]},
    istype_test_util:match(istype_validator, Value, Type1, Expected1, []),

    Type2 = {type, list, {nonempty, {type, any, []}, {type, any, []}}},
    Expected2 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_list}, [Value]},
                    {op, 1, '<',
                        {integer, 1, 0},
                        {call, 1, {atom, 1, length}, [Value]}}},
    istype_test_util:match(istype_validator, Value, Type2, Expected2, []),        

    Type3 = {type, list, {nonempty, {type, any, []}, {literal, {nil, 1}}}},
    Expected3 = {op, 1, 'andalso',
        {call, 1, {atom, 1, is_list}, [Value]},
        {op, 1, '<',
            {integer, 1, 0},
            {call, 1, {atom, 1, length}, [Value]}}},
    istype_test_util:match(istype_validator, Value, Type3, Expected3, []),

    Type4 = {type, list, {maybe_empty, {type, atom, []}, {type, any, []}}},
    ?REMOTE(Value, Type4).

'Map_test'() ->
    Value = {atom, 1, atom},

    Type0 = {type, map, any},
    Expected0 = {call, 1, {atom, 1, is_map}, [Value]},
    istype_test_util:match(istype_validator, Value, Type0, Expected0, []),

    Type1 = {type, map, empty},
    Expected1 = {op, 1, '=:=',
                    Value,
                    {map, 1, []}},
    istype_test_util:match(istype_validator, Value, Type1, Expected1, []),

    Type2 = {type, map, {[{{literal, {atom, 1, key}}, {type, atom, []}}], []}},
    ?REMOTE(Value, Type2).

'Tuple_test'() ->
    Value = {atom, 1, atom},

    Type0 = {type, tuple, any},
    Expected0 = {call, 1, {atom, 1, is_tuple}, [Value]},
    istype_test_util:match(istype_validator, Value, Type0, Expected0, []),

    Type1 = {type, tuple, empty},
    Expected1 = {op, 1, '=:=',
                    Value,
                    {tuple, 1, []}},
    istype_test_util:match(istype_validator, Value, Type1, Expected1, []),

    Type2 = {type, tuple, [{type, atom, []}]},
    Expected2 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_tuple}, [Value]},
                    {op, 1, 'andalso',
                        {op, 1, '=:=',
                            {integer, 1, 1},
                            {call, 1, {atom, 1, size}, [Value]}},
                        {call, 1, {atom, 1, is_atom}, [{call, 1, {atom, 1, element}, [{integer, 1, 1}, Value]}]}}},
    istype_test_util:match(istype_validator, Value, Type2, Expected2, []).

'Union_test'() ->
    Value = {atom, 1, atom},
    
    Type = {type, union, [{type, atom, []}, {literal, {nil, 1}}]},
    Expected = {op, 1, 'orelse',
                   {call, 1, {atom, 1, is_atom}, [Value]},
                   {op, 1, '=:=', Value, {nil, 1}}},
    istype_test_util:match(istype_validator, Value, Type, Expected, []).

record_test() ->
    Value = {atom, 1, atom},
    RecordTypes = #{a => {type, atom, []},
                    b => {type, atom, []},
                    c => {type, range, {{literal, {integer, 1, 0}},
                                        {literal, {integer, 1, 99}}}}},
    RecordDefaults = #{a => {literal, {nil, 1}},
                       b => {literal, {nil, 1}},
                       c => {literal, {nil, 1}}},

    RecordInfo = {3, [a,b,c], RecordTypes, RecordDefaults},
    Records = #{record_a => RecordInfo},

    Type0 = {type, record, {record_a, []}},
    Expected0 = {op, 1, 'andalso',
                    {call, 1, {atom, 1, is_record}, [{atom, 1, atom},
                                                     {atom, 1, record_a},
                                                     {integer, 1, 3}]},
                    {op,1,'andalso',
                        {call, 1, {atom, 1, is_atom}, [{record_field, 1, {atom, 1, atom}, record_a, {atom, 1, a}}]},
                        {op, 1, 'andalso',
                            {call, 1, {atom, 1, is_atom}, [{record_field, 1, {atom, 1, atom}, record_a, {atom, 1, b}}]},
                            {op, 1, 'andalso',
                                {call, 1, {atom, 1, is_integer}, [{record_field, 1, {atom, 1, atom}, record_a, {atom, 1, c}}]},
                                {op, 1, 'andalso',
                                    {op, 1, '>=',
                                        {record_field, 1, {atom, 1, atom}, record_a, {atom, 1, c}},
                                        {integer, 1, 0}},
                                    {op, 1, '=<',
                                        {record_field, 1, {atom, 1, atom}, record_a, {atom, 1, c}},
                                        {integer, 1, 99}}}}}}},
    istype_test_util:match(istype_validator, Value, Type0, Expected0, #{}, Records, []).

custom_test() ->
    Value = {atom, 1, atom},

    Types = #{custom => {type, atom, []}},

    Type = {type, custom, []},
    Expected = {call, 1, {atom, 1, is_atom}, [Value]},
    istype_test_util:match(istype_validator, Value, Type, Expected, Types, #{}, []).










