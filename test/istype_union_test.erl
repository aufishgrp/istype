-module(istype_union_test).

-compile(export_all).

-compile({parse_transform, istype_transform}).

-include("istype.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([]).

-type byte_test() :: [byte()].

range_test() -> ?assertEqual(true, (istype([4], byte_test()))).
