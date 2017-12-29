-module(istype).
-export([istype/2, istype/4,
	     totype/2, totype/4]).

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