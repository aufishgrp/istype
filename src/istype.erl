-module(istype).

%% API exports
-export([return_atom/0]).

-include_lib("eunit/include/eunit.hrl").

-dialyzer({nowarn_function, [invocation_test/0,
                             complex_types_test/0,
                             range_test/0,
                             assert_test/0]}).

-type custom()          :: binary() | integer() | atom.
-type compound_custom() :: custom() | float().
-type range()           :: 1..100.
-type tuple0()          :: {}.
-type tuple1()          :: {atom}.
-type tuple2()          :: {atom, atom()}.
-type tuple3()          :: {atom, atom(), binary()}.
-type tuple_union()     :: tuple1() | tuple3().

-export_type([custom/0,
	          compound_custom/0,
	          range/0,
	          tuple0/0,
	          tuple1/0,
	          tuple2/0,
	          tuple3/0,
	          tuple_union/0]).

%%====================================================================
%% Test functions
%%====================================================================
invocation_test() ->
	TestAtom = atom,
	true = istype(atom, atom()),
	true = istype(TestAtom, atom()),
	true = istype(fun() -> atom end(), atom()),
	true = istype(return_atom(), atom()),
	true = istype(?MODULE:return_atom(), atom()),
	true = istype(case atom of X0 -> X0 end, atom()),
	true = istype(begin return_atom() end, atom()),
	true = case ?MODULE:return_atom() of
	           X1 when istype(X1, atom()) -> true
	       end.

customs_test() ->
	true  = istype(<<"hello">>, custom()),
	true  = istype(1,           custom()),
	true  = istype(atom,        custom()),
	false = istype([],          custom()),
	true  = istype(7.0,         compound_custom()).

range_test() ->
	true  = istype(1,   range()),
	true  = istype(50,  range()),
	true  = istype(100, range()),
	false = istype(0,   range()),
	false = istype(101, range()).
	
assert_test() ->
	asserttype(atom, atom()),
	true = try
	           asserttype(<<"binary">>, atom()),
	           false
	       catch
	           error:{badmatch, false} ->
	               true
	       end.

tuple_test() ->
	true  = istype({}, tuple0()),
	false = istype({1}, tuple0()),

	true  = istype({atom}, tuple1()),
	false = istype({<<"binary">>}, tuple1()),
	false = istype({atom, atom}, tuple1()),

	true  = istype({atom, atom}, tuple2()),
	false = istype({atom, <<"binary">>}, tuple2()),
	false = istype({atom, atom, <<"binary">>}, tuple2()),

	true  = istype({atom, atom, <<"binary">>}, tuple3()),
	false = istype({atom, atom}, tuple3()),
	false = istype({atom, <<"binary">>, atom}, tuple3()),
	false = istype({atom, atom, atom, atom}, tuple3()),

	true  = istype({atom}, tuple_union()),
	true  = istype({atom, atom, <<"binary">>}, tuple_union()),
	false = istype({<<"binary">>}, tuple_union()),
	false = istype({<<"binary">>, <<"binary">>}, tuple_union()),
	false = istype({<<"binary">>, <<"binary">>, <<"binary">>}, tuple_union()).

%%====================================================================
%% Utility functions
%%====================================================================
return_atom() ->
	atom.