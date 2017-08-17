-module(istype).

%% API exports
-export([return_atom/0]).

-include_lib("eunit/include/eunit.hrl").

-dialyzer({nowarn_function, [invocation_test/0,
                             complex_types_test/0,
                             range_test/0,
                             assert_test/0]}).

-type complex_type()          :: binary() | integer() | atom.
-type compound_complex_type() :: complex_type() | float().
-type int_range_type()        :: 1..100.
-type char_range_type()       :: 0..16#ff.
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

complex_types_test() ->
	true  = istype(<<"hello">>, complex_type()),
	true  = istype(1,           complex_type()),
	true  = istype(atom,        complex_type()),
	false = istype([],          complex_type()),
	true  = istype(7.0,         compound_complex_type()).

range_test() ->
	true  = istype(1,      int_range_type()),
	true  = istype(50,     int_range_type()),
	true  = istype(100,    int_range_type()),
	false = istype(0,      int_range_type()),
	false = istype(101,    int_range_type()),
	true  = istype($a,     char_range_type()),
	false = istype(16#fff, char_range_type()).

assert_test() ->
	asserttype(atom, atom()),
	true = try
	           asserttype(<<"binary">>, atom()),
	           false
	       catch
	           error:{badmatch, false} ->
	               true
	       end.

%%====================================================================
%% Utility functions
%%====================================================================
return_atom() ->
	atom.