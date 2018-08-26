-module(istype).
-export([istype/3, istype/5,
         totype/3, totype/5]).

-include("istype.hrl").

-type form()       :: term().
-type forms()      :: list(form()).
-type literal()    :: {literal, form()}.
-type type()       :: #type{} | literal().
-type types()      :: #{mfa() => type()}.
-type type_spec()  :: any | tuple() | list().
-type record()     :: {record, atom(), Arity :: arity(), Fields :: list(atom()), Types :: istype:types()}.
-type records()    :: #{atom() => record()}.

-export_types([form/0, forms/0,
               literal/0,
               type/0, types/0, type_spec/0,
               record/0, records/0]).

%%====================================================================
%% istype functions
%%====================================================================
istype(Value, Type, Options) ->
    istype_lib:istype(Value, Type, #{}, #{}, Options).

istype(Value, Type, Types, Records, Options) ->
    istype_lib:istype(Value, Type, Types, Records, Options).

%%====================================================================
%% totype functions
%%====================================================================
totype(Value, Type, Options) ->
    istype_lib:totype(Value, Type, #{}, #{}, Options).

totype(Value, Type, Types, Records, Options) ->
    istype_lib:totype(Value, Type, Types, Records, Options).