-module(istype).
-export([istype/2, istype/4,
         totype/2, totype/4]).

-type form()      :: erl_parse:abstract_form().
-type forms()     :: list(form()).
-type type()      :: {type, atom(), any | tuple() | list()}.
-type types()     :: #{mfa() => type(),
                       {module(), atom()} => type()}.
-type record()    :: {record, atom(), Arity :: arity(), Fields :: list(atom()), Types :: istype:types()}.
-type records()   :: #{atom() => record()}.

-type options()   :: term().

-export_types([form/0, forms/0,
	           type/0, types/0,
	           record/0, records/0,
	           options/0]).

%%====================================================================
%% istype functions
%%====================================================================
istype(Value, Type) ->
    istype_lib:istype(Value, Type, #{}, #{}).

istype(Value, Type, Types, Records) ->
    istype_lib:istype(Value, Type, Types, Records).

%%====================================================================
%% totype functions
%%====================================================================
totype(Value, Type) ->
    istype_lib:totype(Value, Type, #{}, #{}).

totype(Value, Type, Types, Records) ->
    istype_lib:totype(Value, Type, Types, Records).