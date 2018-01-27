-module(istype_validator_test).

-include_lib("eunit/include/eunit.hrl").

-define(BIF(Type, Bif), fun() ->
                            Value = {atom, 1, atom},
                            Result = {call, 1, {atom, 1, Bif}, [Value]},
                            Result = istype_validator:validate(Value, {type, Type, []}, [])
                        end()).

-define(REMOTE(Value, Type), fun() ->
                                 Abs = erl_parse:abstract(Type, [{line, 1}]),
                                 {call, 1,
                                     {remote, 1, {atom, 1, istype_lib}, {atom, 1, validate}},
                                     [Value,
                                      Abs,
                                      {map, 1, []},
                                      {map, 1, []},
                                      {nil, 1}]} = istype_validator:validate(Value, Type, [])
                             end()).

literal_test() ->
    Value = {atom, 1, atom},
    Literal = {atom, 1, also},
    Type = {literal, Literal},
    Result = {op, 1, '=:=', Value, Literal},
    Result = istype_validator:validate(Value, Type, []).

any_test() ->
    Result = {atom, 1, true},
    Result = istype_validator:validate({atom, 1, atom}, {type, any, []}, []). 

none_test() ->
    Result = {atom, 1, false},
    Result = istype_validator:validate({atom, 1, atom}, {type, none, []}, []). 

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
    {op, 1, '=:=', Value, {bin, 1, []}} = istype_validator:validate(Value, {type, bitstring, {M0, N0}}, []),

    M1 = 7,
    N1 = 0,
    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_bitstring}, [Value]},
        {op, 1, '=:=',
            {call, 1, {atom, 1, bit_size}, [Value]},
            {integer, 1, M1}}} = istype_validator:validate(Value, {type, bitstring, {M1, N1}}, []),

    M2 = 0,
    N2 = 7,
    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_bitstring}, [Value]},
        {op, 1, '=:=',
            {op, 1, 'rem',
                {call, 1, {atom, 1, bit_size}, [Value]},
                {integer, 1, N2}},
            {integer, 1, M2}}} = istype_validator:validate(Value, {type, bitstring, {M2, N2}}, []),

    M3 = 7,
    N3 = 7,
    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_bitstring}, [Value]},
        {op, 1, '=:=',
            {op, 1, 'rem',
                {call, 1, {atom, 1, bit_size}, [Value]},
                {integer, 1, N3}},
            {integer, 1, M3}}} = istype_validator:validate(Value, {type, bitstring, {M3, N3}}, []).

float_test() ->
    ?BIF(float, is_float).

integer_test() ->
    ?BIF(integer, is_integer).

'Integer..Integer_test'() ->
    Value = {atom, 1, atom},

    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_integer}, [Value]},
        {op, 1, 'andalso',
            {op, 1, '>=', Value, {integer, 1, 1}},
            {op, 1, '=<', Value, {integer, 1, 2}}}} = istype_validator:validate(Value, {type, range, {{literal, {integer, 1, 1}}, {literal, {integer, 1, 2}}}}, []),

    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_integer}, [Value]},
        {op, 1, '>=', Value, {integer, 1, 1}}} = istype_validator:validate(Value, {type, range, {{literal, {integer, 1, 1}}, {literal, {atom, 1, undefined}}}}, []),

    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_integer}, [Value]},
        {op, 1, '=<', Value, {integer, 1, 2}}} = istype_validator:validate(Value, {type, range, {{literal, {atom, 1, undefined}}, {literal, {integer, 1, 2}}}}, []).

'List_test'() ->
    Value = {atom, 1, atom},

    Type0 = {type, list, {maybe_empty, {type, any, []}, {type, any, []}}},
    {call, 1, {atom, 1, is_list}, [Value]} = istype_validator:validate(Value, Type0, []),

    Type1 = {type, list, {maybe_empty, {type, any, []}, {literal, {nil, 1}}}},
    {call, 1, {atom, 1, is_list}, [Value]} = istype_validator:validate(Value, Type1, []),

    Type2 = {type, list, {nonempty, {type, any, []}, {type, any, []}}},
    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_list}, [Value]},
        {op, 1, '<',
            {integer, 1, 0},
            {call, 1, {atom, 1, length}, [Value]}}} = istype_validator:validate(Value, Type2, []),

    Type3 = {type, list, {nonempty, {type, any, []}, {literal, {nil, 1}}}},
    {op, 1, 'andalso',
        {call, 1, {atom, 1, is_list}, [Value]},
        {op, 1, '<',
            {integer, 1, 0},
            {call, 1, {atom, 1, length}, [Value]}}} = istype_validator:validate(Value, Type3, []),


    Type4 = {type, list, {maybe_empty, {type, atom, []}, {type, any, []}}},
    ?REMOTE(Value, Type4).

'Map_test'() ->
    Value = {atom, 1, atom},

    Type0 = {type, map, any},
    {call, 1, {atom, 1, is_map}, [Value]} = istype_validator:validate(Value, Type0, []),
    
    Type1 = {type, map, empty},
    {op, 1, '=:=',
        Value,
        {map, 1, []}} = istype_validator:validate(Value, Type1, []),

    Type2 = {type, map, {[{{literal, {atom, 1, key}}, {type, atom, []}}], []}},
    ?REMOTE(Value, Type2).

'Tuple_test'() ->
    Value = {atom, 1, atom},

    Type0 = {type, tuple, any},
    {call, 1, {atom, 1, is_tuple}, [Value]} = istype_validator:validate(Value, Type0, []),

    Type1 = {type, tuple, empty},
    {op, 1, '=:=',
        Value,
        {tuple, 1, []}} = istype_validator:validate(Value, Type1, []).

    %Type2 = {type, tuple, [{type, atom, []}]},
    %{op, 1, 'andalso',
    %    {call, 1, {atom, 1, is_tuple}, [Value]},
    %    {op, 1, 'andalso',
    %        {op, 1, '=:=',
    %            {integer, 1, }}}}

record_test() ->
    Types = #{a => {type, atom, []},
              b => {type, atom, []},
              c => {type, range, {{literal, {integer, 1, 0}},
                                  {literal, {integer, 1, 99}}}}},

    Defaults = #{a => {literal, {nil, 1}},
                 b => {literal, {nil, 1}},
                 c => {literal, {nil, 1}}},

    RecordInfo = {3, [a,b,c], Types, Defaults},
    Forms = istype_validator:validate({atom, 1, atom}, {type, record, {test, []}}, #{}, #{test => RecordInfo}, []),
    io:format("\n~s\n", [forms:from_abstract(Forms)]),
    ok = notok.    














