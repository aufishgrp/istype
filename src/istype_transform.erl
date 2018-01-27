-module(istype_transform).
-export([parse_transform/2, get_var/1]).

%%====================================================================
%% parse_transform api
%%====================================================================
log_parse_type(_) -> ok.

parse_transform(Forms0, _Options) ->
    %try
        %%io:format("Forms\n~p\n", [Forms0]),
        Forms0.
    %    {attribute, _, module, Module} = lists:keyfind(module, 3, Forms0),
    %    Records = forms:reduce(fun get_records/2, #{}, Forms0),
    %    Types = forms:reduce(fun get_types/2,
    %                         #{iolist => log_parse_type({type, 1, iolist, []})},
    %                         Forms0),

    %    Forms1 = add_default_records(Forms0, Records),
    %    forms:map(fun(Form) ->
    %                  do_transform(Module, Form, Types, Records)
    %              end,
    %              Forms1)
    %catch
    %    Class:Error ->
    %        handle_error(Class, Error, erlang:get_stacktrace()),
    %
    %        halt(1)
    %end.

do_transform(_, {call, Line, {atom, _, istype}, [Value, Type]}, Types, Records) ->
    %% is_integer(Value) orelse is_boolean(Value) ...
    %%
    %% OR
    %%
    %% begin
    %%     __IsType_1 = expression(),
    %%     is_integer(__IsType_1) orelse is_boolean(__IsType_1) ...
    %% end
    istype_validator:validate(Value, istype_parser:parse_type(Types, Records), Types, Records, []);
do_transform(Module, {call, Line, {atom, _, totype}, [Value, Type0]}, Types, Records) ->
    %% try
    %%     __IsType_1 = istype_lib:totype(Value, TypeInfo),
    %%     asserttype(__IsType_1, type()),
    %%     __IsTYpe_1
    %% catch
    %%     error:{badmatch, false} ->
    %%         error({istype_conversion, type(), __IsType_1})
    %% end
    Type1 = log_parse_type(Type0),

    Type2 = case Type1 of
                {record, _, _} = Record ->
                    record_to_record_spec(Module, Record, Types, Records);
                _ ->
                    Type1
            end,

    %io:format("++++++++++++++++\nToType\n~p\n~p\n~p\n----------------\n", [Type0, Type1, Type2]),

    Converted = get_var(Line),
    {'try', Line,
        [{match, Line,
             Converted,
             {call, Line,
                 {remote, Line,
                     {atom, Line, istype_lib},
                     {atom, Line, totype}},
                 [Value,
                  erl_parse:abstract(Type2, [{line, Line}]),
                  erl_parse:abstract(Types, [{line, Line}]),
                  erl_parse:abstract(Records, [{line, Line}])]}},
         do_transform(Module,
                      {call, Line,
                          {atom, Line, asserttype},
                          [Converted, Type0]},
                      Types,
                      Records),
         Converted],
        [],
        [{clause, Line,
             [{tuple, Line,
                  [{atom, Line, error},
                   {tuple, Line,
                       [{atom, Line, badmatch},
                        {atom, Line, false}]},
                   {var, Line, '_'}]}],
             [],
             [{call, Line,
                  {atom, Line, error},
                  [{tuple, Line, [{atom, Line, istype_conversion},
                                  erl_parse:abstract(Type2, [{line, Line}]),
                                  Value,
                                  {atom, Line, invalid_result}]}]}]}],
        []};
do_transform(Module, {call, Line, {atom, _, asserttype}, Args}, Types, Records) ->
    %% true = istype(Value, type())
    {match, Line,
        {atom, Line, true},
        do_transform(Module, {call, Line, {atom, Line, istype}, Args}, Types, Records)};
do_transform(_, Form, _, _) ->
    Form.

handle_error(Class, Error, Stack) ->
    erlang:Class({Class, Error, Stack}).



%%====================================================================
%% default_records functions
%%====================================================================
%% default_records
%%==========================================================
add_default_records(Forms, Records) ->
    Last = element(2, hd(lists:reverse(Forms))),
    {Export, Fun} = default_records(Last, Records),
    maybe_add_default_records(Last, Export, Fun, Forms).

default_records(Line, Records) ->
    Export = {attribute, line, export, [{istype_default_records, 1}]},
    Clauses = maps:fold(fun(Record, _, Acc) ->
                            Clause = {clause, Line, [{atom, Line, Record}], [], [{record, Line, Record, []}]},
                            [Clause | Acc]
                        end,
                        [],
                        Records),

    case Clauses of
        [] ->
            {undefined, undefined};
        _ ->
            Fun = {function, Line, istype_default_records, 1, Clauses},
            {Export, Fun}
    end.

maybe_add_default_records(_, undefined, undefined, Forms) ->
    Forms;
maybe_add_default_records(Last, Export0, Fun, Forms0) ->
    {_, Forms1} = lists:foldl(fun({eof, _}, {false, Acc}) ->
                                     {false, Acc};
                                 (Form, {false, Acc}) ->
                                     {false, [Form | Acc]};
                                 (Form, {true, Acc}) ->
                                     case Form of
                                         {attribute, Line, export, _} ->
                                             Export1 = setelement(2, Export0, Line),
                                             {false, [Export1, Form | Acc]};
                                         _ ->
                                             {true, [Form | Acc]}
                                     end
                              end,
                              {true, []},
                              Forms0),
    lists:reverse([{eof, Last + 1}, Fun | Forms1]).

%%====================================================================
%% utility functions
%%====================================================================
%% @doc generates a variable name that should be conflict free.
%% @end
get_var(Line) ->
    Var = case get('__var_counter__') of
              undefined -> 1;
              Counter -> Counter + 1
          end,
    put('__var_counter__', Var),
    {var, Line, list_to_atom("__IsType_" ++ integer_to_list(Var))}.

%% @doc Function that generates a mapping of types to type specs.
%% @end
get_types({attribute, _, type, {Type, {remote_type, _, _} = TypeSpec, _} = X}, Acc) ->
  Acc;
get_types({attribute, _, type, {Type, TypeSpec, _} = X}, Acc) ->
  Acc#{Type => log_parse_type(TypeSpec)};
get_types(_, Acc) ->
  Acc.

%% @doc Function that generates a mapping of records to record specs.
%% @end
get_records({attribute, _, record, {Record, RecordFields}} = R, Acc) ->
    %io:format("Parsing Record\n~p\n", [R]),
    %{attribute,23,record,
    %    {record_a,
    %        [{typed_record_field,
    %             {record_field,23,{atom,23,a},{atom,23,atom}},
    %             {type,23,atom,[]}},
    %         {record_field,24,{atom,24,b},{atom,24,atom}},
    %         {typed_record_field,{record_field,25,{atom,25,c}},{type,25,atom,[]}},
    %         {record_field,26,{atom,26,d}},
    %         {typed_record_field,
    %             {record_field,27,{atom,27,e}},
    %             {type,27,union,[{type,27,atom,[]},{type,27,binary,[]}]}}]}}
    Acc;%Acc#{Record => parse_record_fields(RecordFields)};
get_records(_, Acc) ->
    Acc.

record_to_record_spec(Module, Record, Types, Records) ->
    {record_spec, RecordLabel, RecordProperties} = record_to_record_spec(Record, Types, Records),
    {record_spec, RecordLabel, RecordProperties, Module}.

record_to_record_spec({record, Record, Overrides}, Types, Records) ->
    {record_spec, Record, override_record_spec(Overrides, Record, Types, Records)}.

%% @doc Fetches a record spec with the overrides applied.
%% @end
override_record_spec(Overrides, Record, _, Records) ->
    #{Record := {Arity, RecordFields, RecordTypes}} = Records,
    DefaultTypes = lists:zip(RecordFields, RecordTypes),

    %% Update types
    Overridden = lists:foldl(fun({K, V}, Acc) ->
                                 lists:keyreplace(K, 1, Acc, {K, V})
                             end,
                             DefaultTypes,
                             Overrides),

    {_, OverriddenTypes} = lists:unzip(Overridden),
    {Arity, RecordFields, OverriddenTypes, Overridden}.
