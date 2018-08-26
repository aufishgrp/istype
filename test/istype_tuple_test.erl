-module(istype_tuple_test).

-compile({parse_transform, istype_transform}).

-include("istype.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([]).

-export_type([any_tuple/0,
              empty_tuple/0,
              fully_typed_tuple/0,
              partially_typed_tuple/0]).

rv(X) -> X.

%%=====================================
%% Tuple
%%=====================================
%% @doc Tuple :: tuple() %% Any tuple
%%             | {}      %% Empty tuple
%%             | {TList}
%%
%%      TList :: Type
%%             | Type, TList
%% @end
-type any_tuple()              :: tuple().
-type empty_tuple()            :: {}.
-type fully_typed_tuple()      :: {atom(), binary()}.
-type partially_typed_tuple()  :: {atom(), _}.

is_tuple_test_() ->
    [?_assertEqual(true,  istype({},     tuple())),
     ?_assertEqual(true,  istype({atom}, tuple())),
     ?_assertEqual(false, istype([],     tuple()))].

is_empty_tuple_test_() ->
    [?_assertEqual(true,  istype({},     {})),
     ?_assertEqual(false, istype({atom}, {})),
     ?_assertEqual(false, istype([],     {}))].

is_aliased_tuple_test_() ->
    [?_assertEqual(true,  istype({},     any_tuple())),
     ?_assertEqual(true,  istype({atom}, any_tuple())),
     ?_assertEqual(false, istype([],     any_tuple())),

     ?_assertEqual(true,  istype({},     empty_tuple())),
     ?_assertEqual(false, istype({atom}, empty_tuple())),
     ?_assertEqual(false, istype([],     empty_tuple()))].

is_fully_typed_tuple_test_() ->
    [?_assertEqual(false, istype(rv([]),               fully_typed_tuple())),
     ?_assertEqual(false, istype(rv({}),               fully_typed_tuple())),
     ?_assertEqual(false, istype({atom, binary},       fully_typed_tuple())),
     ?_assertEqual(true,  istype({atom, <<"binary">>}, fully_typed_tuple()))].

is_partially_typed_tuple_test_() ->
    [?_assertEqual(false, istype(rv([]),               partially_typed_tuple())),
     ?_assertEqual(false, istype(rv({}),               partially_typed_tuple())),
     ?_assertEqual(true,  istype({atom, binary},       partially_typed_tuple())),
     ?_assertEqual(true,  istype({atom, <<"binary">>}, partially_typed_tuple())),
     ?_assertEqual(false, istype({<<"binary">>, atom}, partially_typed_tuple()))].

to_tuple_test_() ->
    [?_assertEqual({},                           totype({},                      tuple())),
     ?_assertEqual({},                           totype([],                      tuple())),
     ?_assertEqual({atom, <<"binary">>, 1},      totype({atom, <<"binary">>, 1}, tuple())),
     ?_assertEqual({atom, <<"binary">>, 1},      totype([atom, <<"binary">>, 1], tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype(atom,                    tuple()))].

to_aliased_tuple_test_() ->
    [?_assertEqual({},                           totype({},                      any_tuple())),
     ?_assertEqual({},                           totype([],                      any_tuple())),
     ?_assertEqual({atom, <<"binary">>, 1},      totype({atom, <<"binary">>, 1}, any_tuple())),
     ?_assertEqual({atom, <<"binary">>, 1},      totype([atom, <<"binary">>, 1], any_tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype(atom,                    any_tuple()))].

to_empty_tuple_test_() ->
    [?_assertEqual({},                           totype({},                      empty_tuple())),
     ?_assertEqual({},                           totype([],                      empty_tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype({atom, <<"binary">>, 1}, empty_tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype(atom,                    empty_tuple()))].

to_fully_typed_tuple_test_() ->
    [?_assertEqual({atom, <<"binary">>},         totype({atom, <<"binary">>},   fully_typed_tuple())),
     ?_assertEqual({atom, <<"binary">>},         totype([atom, <<"binary">>],   fully_typed_tuple())),
     ?_assertEqual({atom, <<"binary">>},         totype({"atom", "binary"},     fully_typed_tuple())),
     ?_assertEqual({atom, <<"binary">>},         totype(["atom", <<"binary">>], fully_typed_tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype({1, <<"binary">>},      fully_typed_tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype(atom,                   fully_typed_tuple()))].

to_partially_typed_tuple_test_() ->
    [?_assertEqual({atom, <<"binary">>},         totype({atom, <<"binary">>},   partially_typed_tuple())),
     ?_assertEqual({atom, <<"binary">>},         totype([atom, <<"binary">>],   partially_typed_tuple())),
     ?_assertEqual({atom, "binary"},             totype({"atom", "binary"},     partially_typed_tuple())),
     ?_assertEqual({atom, binary},               totype([<<"atom">>, binary],   partially_typed_tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype({1, <<"binary">>},      partially_typed_tuple())),
     ?_assertError({istype_conversion, _, _, _}, totype(atom,                   partially_typed_tuple()))].
