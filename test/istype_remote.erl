-module(istype_remote).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, istype_transform}).

-type type_a()  :: atom().

-type type_0(A) :: {A}.
-type type_1(B) :: type_0(B).
-type type_2()  :: type_1(atom()).
-type type_3()  :: {atom(), type_1(binary()), istype:type()}.