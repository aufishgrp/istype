-module(istype_transform).
-export([parse_transform/2]).
-export([update_term/2, substitute_literals/1]).
-include("istype.hrl").

%%====================================================================
%% parse_transform api
%%====================================================================
-spec parse_transform(istype:forms(), term()) -> istype:forms().
parse_transform(Forms0, Options) ->
    {attribute, _, module, Module} = lists:keyfind(module, 3, Forms0),
    {attribute, Export, export, _} = lists:keyfind(export, 3, Forms0),

    Types0 = istype_parser:parse_types(Forms0),
    {Types1, Records} = istype_parser:parse_records(Forms0, Types0),
    
    Forms1 = forms:map(fun(Form) ->
                           do_transform(Module, Form, Types1, Records, Options)
                       end,
                       Forms0),

    [{eof, EOFLine} | Forms2] = lists:reverse(Forms1),

    Forms3 = lists:reverse([{eof, EOFLine + 3},
                            src_fun(EOFLine + 2, Module),
                            records_fun(EOFLine + 1, Records),
                            types_fun(EOFLine, Types1) | 
                            Forms2]),

    Forms4 = forms:map(fun(#literal{value = Value}) ->
                              Value;
                          (Form) ->
                              Form
                       end,
                       Forms3),

    insert_at_line(Export, 
                   {attribute, Export, export, [{istype_to_src, 0},
                                                {istype_types, 0},
                                                {istype_records, 0}]},
                   Forms4).
    

do_transform(Module, {call, Line, {atom, _, istype}, [Value, Type0]}, Types, Records, Options) ->
    Type1 = istype_parser:resolve_type(istype_parser:parse_type(Module, Type0), Types),
    istype_validator:transform(Line, Value, Type1, Types, Records, Options);

do_transform(Module, {call, Line, {atom, _, totype}, [Value, Type0]}, Types, Records, Options) ->
    Type1 = istype_parser:resolve_type(istype_parser:parse_type(Module, Type0), Types),
    istype_converter:transform(Line, Value, Type1, Types, Records, Options);    

do_transform(Module, {call, Line, {atom, _, asserttype}, Args}, Types, Records, Options) ->
    {match, Line,
        {atom, Line, true},
        do_transform(Module, {call, Line, {atom, Line, istype}, Args}, Types, Records, Options)};

do_transform(_, Form, _, _, _) ->
    Form.

insert_at_line(Line, Form, Forms) ->
    Head = lists:takewhile(fun(X) -> element(2, X) =< Line end, Forms),
    Tail = lists:dropwhile(fun(X) -> element(2, X) < Line + 1 end, Forms),
    Head ++ [Form] ++ Tail.

types_fun(Line, Types) ->
    istype_fun(istype_types, Line, Types).

records_fun(Line, Records) -> 
    istype_fun(istype_records, Line, Records).

istype_fun(Function, Line, Value) ->
    {function, Line, Function, 0,
        [{clause, Line, [], [],
              [erl_parse:abstract(Value)]}]}.

src_fun(Line, Module) ->
    {function, Line, istype_to_src, 0,
        [{clause, Line, [], [],
              [{call, Line, {remote, Line, {atom, Line, io_lib}, {atom, Line, format}},
                    [erl_parse:abstract("~s\n", [{line, Line}]),
                     {cons, Line, {call, Line, {remote, Line, {atom, Line, forms}, {atom, Line, read}},
                                       [erl_parse:abstract(Module, [{line, Line}])]},
                                  {nil, Line}}]}]}]}.

%%=========================================================
%% update_term
%%=========================================================
update_term(Fun, Term0) ->
    case Fun(Term0) of
        Term0 when is_list(Term0) ->
            [update_term(Fun, X) || X <- Term0];
        Term0 when is_tuple(Term0) ->
            list_to_tuple([update_term(Fun, X) || X <- tuple_to_list(Term0)]);
        Term0 when is_map(Term0) ->
            {K, V} = lists:unzip(maps:to_list(Term0)),
            maps:from_list(lists:zip([update_term(Fun, X) || X <- K],
                                     [update_term(Fun, X) || X <- V]));
        Term1 ->
            Term1
    end.

%%=========================================================
%% substitute_literals
%%=========================================================
%% @doc Subtitutes #literal{} with the literal value in the terms provided.
%% @end
substitute_literals(Term) ->
    update_term(fun(#literal{value = Value}) ->
                       #literal{value = erl_parse:normalise(Value)};
                   (X) ->
                       X
                end,
                Term).

