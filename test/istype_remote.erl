-module(istype_remote).

-compile({parse_transform, istype_transform}).

-export([t/0]).

-type type_a()     :: atom().
-type type_b()     :: [atom()].

-type type_0(A, B) :: {A, B}.
-type type_1(A)    :: type_0(A, binary()).
-type type_2()     :: type_1(atom()).
-type type_3()     :: {atom(), type_1(binary())}.

-record(record_a, {a = atom :: atom()}).

-export_type([type_a/0,
	          type_0/2, type_1/1, type_2/0, type_3/0]).

t() ->
	#record_a{},
	#record_a{a = also}.