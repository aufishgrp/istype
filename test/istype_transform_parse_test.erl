-module(istype_transform_parse_test).

-include_lib("eunit/include/eunit.hrl").

any_test() ->
    %% As Type
    {type, any, []} = istype_transform:parse_type({type, 1, any, []}),
    %% As Call
    {type, any, []} = istype_transform:parse_type({call, 1, {atom, 1, any}, []}).

none_test() ->
    %% As Type
    {type, none, []} = istype_transform:parse_type({type, 1, none, []}),
    %% As Call
    {type, none, []} = istype_transform:parse_type({call, 1, {atom, 1, none}, []}).

pid_test() ->
    %% As Type
    {type, pid, []} = istype_transform:parse_type({type, 1, pid, []}),
    %% As Call
    {type, pid, []} = istype_transform:parse_type({call, 1, {atom, 1, pid}, []}).

port_test() ->
    %% As Type
    {type, port, []} = istype_transform:parse_type({type, 1, port, []}),
    %% As Call
    {type, port, []} = istype_transform:parse_type({call, 1, {atom, 1, port}, []}).

reference_test() ->
    %% As Type
    {type, reference, []} = istype_transform:parse_type({type, 1, reference, []}),
    %% As Call
    {type, reference, []} = istype_transform:parse_type({call, 1, {atom, 1, reference}, []}).

nil_test() ->
    %% As Type
    {type, nil, []} = istype_transform:parse_type({type, 1, nil, []}),
    %% As Call
    {type, nil, []} = istype_transform:parse_type({call, 1, {atom, 1, nil}, []}),
    %% As literal
    {literal, nil, [], {nil, 1}} = istype_transform:parse_type({nil, 1}),

atom_test() ->
    %% As Type
    {type, atom, []} = istype_transform:parse_type({type, 1, atom, []}),
    %% As Call
    {type, atom, []} = istype_transform:parse_type({call, 1, {atom, 1, atom}, []}).

'ErlangAtom_test'() ->
    %% As Literal
    {literal, atom, 'ErlangAtom', {atom, 1, 'ErlangAtom'}} = istype_transform:parse_type({atom, 1, 'ErlangAtom'}).

'Bitstring_test'() ->
    {type, bitstring, {0, 0}} = istype_transform:parse_type({bin, 1, []}),
    {type, bitstring, {1, 2}} = istype_transform:parse_type({type, 1, binary, [{integer, 1, 1},
                                                                               {integer, 1, 2}]}).

float_test() ->
    {type, float, []} = istype_transform:parse_type({type, 1, float, []}),
    {type, float, []} = istype_transform:parse_type({call, 1, {atom, 1, float}, []}).

integer_test() ->
    {type, integer, []} = istype_transform:parse_type({type, 1, integer, []}),
    {type, integer, []} = istype_transform:parse_type({call, 1, {atom, 1, integer}, []}).

'Integer_test'() ->
    {literal, integer, 1, {integer, 1, 1}} = istype_transform:parse_type({integer, 1, 1}),
    {literal, integer, -1, {op, 1, '-', {integer, 1, 1}} = istype_transform:parse_type({op, 1, '-', {integer, 1, 1}}).

'Integer..Integer_test'() ->
    {type, range, {1, 2}} = istype_transform:parse_type({type, 1, range, [{integer, 1, 1},
                                                                          {integer, 1, 2}]}).

'List_test'() ->
    {type, list, any} = istype_transform:parse_type({type, 1, list, [{type, 1, any, []}]}),
    {type, list, any} = istype_transform:parse_type({call, 1, {atom, 1, list}, [{type, 1, any, []}]}),

    {type, list, {maybe_empty, {type, integer, []}, {literal, nil, []}}} = istype_transform:parse_type({type, 1, list, [{type, 1, integer, []}]}),
    {type, list, {maybe_empty, {type, integer, []}, {literal, nil, []}}} = istype_transform:parse_type({call, 1, {atom, 1, list}, [{call, 1, {atom, 1, integer}, []}]}),
    
    {type, list, {maybe_empty, {type, integer, []}, {type, any, []}}} = istype_transform:parse_type({type, 1, maybe_improper_list, [{type, 1, integer, []}, {type, 1, any, []}]}),
    {type, list, {maybe_empty, {type, integer, []}, {type, any, []}}} = istype_transform:parse_type({call, 1, {atom, 1, maybe_improper_list}, [{call, 1, {atom, 1, integer}, []}, {call, 1, {atom, 1, any}, []}]}),
    
    {type, list, {nonempty, {type, integer, []}, {type, any, []}}} = istype_transform:parse_type({type, 1, nonempty_improper_list, [{type, 1, integer, []}, {type, 1, any, []}]}),
    {type, list, {nonempty, {type, integer, []}, {type, any, []}}} = istype_transform:parse_type({call, 1, {atom, 1, nonempty_improper_list}, [{call, 1, {atom, 1, integer}, []}, {call, 1, {atom, 1, any}, []}]}),
    
    {type, list, {nonempty, {type, any, []}, {literal, nil, []}}} = istype_transform:parse_type({type, 1, nonempty_list, [{type, 1, any, []}]}),
    {type, list, {nonempty, {type, any, []}, {literal, nil, []}}} = istype_transform:parse_type({call, 1, {atom, 1, nonempty_list}, [{type, 1, any, []}]}).

'Map_test'() ->
    {type, map, any} = istype_transform:parse_type({type, 1, map, any}),
    {type, map, any} = istype_transform:parse_type({call, 1, {atom, 1, map}, []}),

    {type, map, empty} = istype_transform:parse_type({type, 1, map, []}),

    {type, map, {[{{type, atom, []},
                   {type, atom, []}}],
                 []}} = istype_transform:parse_type({type, 1, map, [{type, 1, map_field_exact, [{type, 1, atom, []}, {type, 1, atom, []}]}]}),

    {type, map, {[],
                 [{{type, binary, []},
                   {type, binary, []}}]}} = istype_transform:parse_type({type, 1, map, [{type, 1, map_field_assoc, [{type, 1, binary, []}, {type, 1, binary, []}]}]}),

    {type, map, {[{{type, atom, []},
                   {type, atom, []}}],
                 [{{type, binary, []},
                   {type, binary, []}}]}} = istype_transform:parse_type({type, 1, map, [{type, 1, map_field_exact, [{type, 1, atom, []}, {type, 1, atom, []}]},
                                                                                        {type, 1, map_field_assoc, [{type, 1, binary, []}, {type, 1, binary, []}]}]}),

                   {literal, map, }










