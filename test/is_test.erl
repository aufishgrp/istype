-module(is_test).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, istype_transform}).

-type type_a(A) :: {A}.
-type type_b() :: type_a(atom()).
-type type_c() :: forms:abstract_form().

-opaque opaque_a(A) :: {A}.
-opaque opaque_b() :: opaque_a(atom()).
-opaque opaque_c() :: forms:abstract_form().

function() ->
	true = istype(atom, forms:abstract_form()).