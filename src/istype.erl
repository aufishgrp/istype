-module(istype).

%% API exports
-export([return_atom/0]).

-include_lib("eunit/include/eunit.hrl").

-record(record0, {a = apple  :: atom(),
	              b = <<"">> :: binary(),
	              c          :: undefined | range(),
	              d}).

-record(record1, {a = #record0{} :: record0(),
	              b = <<"">> :: binary()}).

-type custom()          :: binary() | integer() | atom.
-type compound_custom() :: custom() | float().
-type range()           :: 1..100.
-type tuple0()          :: {}.
-type tuple1()          :: {atom}.
-type tuple2()          :: {atom, atom()}.
-type tuple3()          :: {atom, atom(), binary()}.
-type tuple_union()     :: tuple1() | tuple3().
-type record0()         :: #record0{}.
-type record1()         :: #record1{}.

-export_type([custom/0,
	          compound_custom/0,
	          range/0,
	          tuple0/0,
	          tuple1/0,
	          tuple2/0,
	          tuple3/0,
	          tuple_union/0,
	          record0/0,
	          record1/0]).

-define(validate(Expected, Type), lists:foreach(fun(Value) ->
	                                                io:format("~p = istype(~p, ~p())\n", [Expected, Value, Type]),
	                                                Expected = istype(Value, Type())
	                                            end,
	                                            Type(Expected))).

%%====================================================================
%% Test functions
%%====================================================================
assert_test() ->
	true = asserttype(atom, atom()),
	true = try
	           asserttype(<<"binary">>, atom()),
	           false
	       catch
	           error:{badmatch, false} ->
	               true
	       end.

invocation_test() ->
	TestAtom = atom,
	TestRecord = #record0{},
	true = istype(TestRecord#record0.a, atom()),
	true = istype(atom, atom()),
	true = istype(TestAtom, atom()),
	true = istype(fun() -> atom end(), atom()),
	true = istype(return_atom(), atom()),
	true = istype(?MODULE:return_atom(), atom()),
	true = istype(case atom of X0 -> X0 end, atom()),
	true = istype(begin return_atom() end, atom()),
	true = case ?MODULE:return_atom() of
	           X1 when istype(X1, atom()) -> true
	       end,
	true = fun(X2) when istype(X2, atom()) -> true;
		      (_) -> false
		   end(TestAtom).

custom_type_test() ->
	true  = istype(<<"hello">>, custom()),
	true  = istype(1,           custom()),
	true  = istype(atom,        custom()),
	false = istype([],          custom()),
	true  = istype(7.0,         compound_custom()),
	true  = case 7.0 of
	            X when istype(X, compound_custom()) -> true;
	            _ -> false
	        end.

range_test() ->
	true  = istype(1,   range()),
	true  = istype(50,  range()),
	true  = istype(100, range()),
	false = istype(0,   range()),
	false = istype(101, range()),
	true  = case 10 of
	            X when istype(X, range()) -> true;
	            _ -> false
	        end.

record_test() ->
	?validate(true, record0),
	?validate(false, record0),

	?validate(true, record1),
	?validate(false, record1).

tuple_test() ->
	?validate(true,  tuple0),
	?validate(false, tuple0),

	?validate(true,  tuple1),
	?validate(false, tuple1),

	?validate(true,  tuple2),
	?validate(false, tuple2),

	?validate(true,  tuple3),
	?validate(false, tuple3),

	?validate(true,  tuple_union),
	?validate(false, tuple_union),

	true  = case hd(tuple3(true)) of
		        X when istype(X, tuple3()) -> true;
		        _ -> false
	        end. 


%%====================================================================
%% Utility functions
%%====================================================================
return_atom() ->
	atom.

record0(true) ->
	[#record0{},
	 #record0{a = also,
	          b = <<"also">>,
	          c = 50}];
record0(false) ->
	[<<"">>, {}, #record0{c = atom}, {record0}, #record1{}].

record1(true) ->
	[#record1{},
	 #record1{b = <<"also">>}];
record1(false) ->
	[<<"">>, {}, #record0{}, {record0}, #record1{a = undefined}].

tuple0(true) -> 
	[{}];
tuple0(false) ->
	[<<"">>, {1}].

tuple1(true) ->
	[{atom}];
tuple1(false) ->
	[<<"">>, {1}, {1, 2}].

tuple2(true) ->
	[{atom, atom}, {atom, also}];
tuple2(false) ->
	[<<"">>, {atom}, {also, also}, {atom, <<"binary">>}, {atom, atom, <<"binary">>}].

tuple3(true) ->
	[{atom, atom, <<"binary">>}, {atom, also, <<"also">>}];
tuple3(false) ->
	[<<"">>, {atom, <<"binary">>}, {also, atom, <<"binary">>}, {atom, atom, atom}, {atom, atom, <<"binary">>, <<"binary">>}].

tuple_union(true) ->
	[{atom}, {atom, atom, <<"binary">>}, {atom, also, <<"also">>}];
tuple_union(false) ->
	[<<"">>, {}, {<<"">>}, {atom, atom}, {also, also, also}, {atom, atom, atom, atom}].

