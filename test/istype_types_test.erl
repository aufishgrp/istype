-module(istype_types_test).

-record(recorda, {a = 0 :: integer() | float()}).

-type typea() :: atom().
-type recorda() :: #recorda{}.
-type recordb() :: #recorda{a :: float()}.
-export_types([typea/0]).

