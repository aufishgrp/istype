-module(istype_transform).
-export([parse_transform/2]).

%%====================================================================
%% parse_transform api
%%====================================================================
parse_transform(Forms, Options) ->
    io:format("Found Forms\n~p\n", [Forms]),

    Types0 = istype_parser:parse_types(Forms),
    {Types1, Records} = istype_parser:parse_records(Forms, Types0),

    forms:map(fun(Form) ->
                  do_transform(Form, Types1, Records, Options)
              end,
              Forms).

do_transform({call, _, {atom, _, istype}, [Value, Type]}, Types, Records, Options) ->
    istype_validator:transform(Value, istype_parser:parse_type(Type), Types, Records, Options);

do_transform({call, _, {atom, _, totype}, [Value, Type]}, Types, Records, Options) ->
    istype_converter:transform(Value, istype_parser:parse_type(Type), Types, Records, Options);    

do_transform({call, Line, {atom, _, asserttype}, Args}, Types, Records, Options) ->
    {match, Line,
        {atom, Line, true},
        do_transform({call, Line, {atom, Line, istype}, Args}, Types, Records, Options)};

do_transform(Form, _, _, _) ->
    Form.
