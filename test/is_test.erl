-module(is_test).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, istype_transform}).

-type type_a(A) :: {A}.
-type type_b() :: type_a(atom()).