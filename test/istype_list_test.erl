-module(istype_list_test).

-compile(export_all).

-compile({parse_transform, istype_transform}).

-include("istype.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([]).

-export_type([any_list/0, empty_list/0, non_empty_list/0, typed_list/0, typed_nonempty_list/0]).

%%=====================================
%% list()
%%=====================================
%% @doc list() :: [any()]
%% @end
-type any_list() :: list().

-type empty_list() :: [].

-type non_empty_list() :: nonempty_list().

-type typed_list() :: [atom()].

-type typed_nonempty_list() :: [atom(), ...].

is_list_test_() ->
    [?_assertEqual(true, (istype([], list()))), ?_assertEqual(true, (istype([1], list()))), ?_assertEqual(false, (istype(atom, list())))].

is_aliased_list_test_() ->
    [?_assertEqual(true, (istype([], any_list()))), ?_assertEqual(true, (istype([1], any_list()))),
     ?_assertEqual(false, (istype(atom, any_list())))].

is_empty_list_test_() ->
    [?_assertEqual(true, (istype([], empty_list()))), ?_assertEqual(false, (istype([1], empty_list()))),
     ?_assertEqual(false, (istype(atom, empty_list())))].

is_typed_list_test_() ->
    [?_assertEqual(true, (istype([], typed_list()))), ?_assertEqual(true, (istype([atom], typed_list()))),
     ?_assertEqual(false, (istype([1], typed_list()))), ?_assertEqual(false, (istype(atom, typed_list())))].

is_nonempty_list_test_() ->
    [?_assertEqual(false, (istype([], nonempty_list()))), ?_assertEqual(true, (istype([1], nonempty_list()))),
     ?_assertEqual(false, (istype({}, nonempty_list())))].

is_aliased_nonempty_list_test_() ->
    [?_assertEqual(false, (istype([], non_empty_list()))), ?_assertEqual(true, (istype([1], non_empty_list()))),
     ?_assertEqual(false, (istype({}, non_empty_list())))].

to_list_test_() ->
    {setup, fun () -> open_port({fd, 1, 1}, []) end, fun (Port) -> port_close(Port) end,
     fun (Port) ->
	     Pid = self(),
	     Ref = make_ref(),
	     Fun = fun () -> ok end,
	     [?_assertEqual([], (totype([], list()))), ?_assertEqual([1], (totype([1], list()))),
	      ?_assertEqual((pid_to_list(Pid)), (totype(Pid, list()))), ?_assertEqual((port_to_list(Port)), (totype(Port, list()))),
	      ?_assertEqual((ref_to_list(Ref)), (totype(Ref, list()))), ?_assertEqual("atom", (totype(atom, list()))),
	      ?_assertEqual("", (totype(<<"">>, list()))), ?_assertEqual("bitstring", (totype(<<"bitstring">>, list()))),
	      ?_assertEqual("binary", (totype(<<"binary">>, list()))), ?_assertEqual("1.10000000000000008882", (totype(1.1, list()))),
	      ?_assertEqual("123456789", (totype(123456789, list()))), ?_assertEqual([], (totype(#{}, list()))),
	      ?_assertEqual([{1, 1}], (totype(#{1 => 1}, list()))), ?_assertEqual((erlang:fun_to_list(Fun)), (totype(Fun, list()))),
	      ?_assertEqual([], (totype({}, list()))), ?_assertEqual([1], (totype({1}, list())))]
     end}.

to_typed_list_test_() ->
    [?_assertEqual([], (totype([], typed_list()))), ?_assertEqual([atom], (totype([atom], typed_list()))),
     ?_assertEqual([atom], (totype([<<"atom">>], typed_list()))), ?_assertEqual([atom, atom], (totype([<<"atom">>, "atom"], typed_list()))),
     ?_assertEqual([], (totype({}, typed_list()))), ?_assertEqual([atom], (totype({atom}, typed_list()))),
     ?_assertEqual([atom], (totype({<<"atom">>}, typed_list()))), ?_assertEqual([atom, atom], (totype({<<"atom">>, "atom"}, typed_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype(<<"binary">>, typed_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype([1, 2], typed_list())))].

to_nonempty_list_test_() ->
    [?_assertEqual([atom], (totype([atom], nonempty_list()))), ?_assertEqual([atom], (totype({atom}, nonempty_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype([], nonempty_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype({}, nonempty_list())))].

to_typed_nonempty_list_test_() ->
    [?_assertEqual([atom], (totype([atom], typed_nonempty_list()))), ?_assertEqual([atom], (totype({atom}, typed_nonempty_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype([1], typed_nonempty_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype({1}, typed_nonempty_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype([], typed_nonempty_list()))),
     ?_assertError({istype_conversion, _, _, _}, (totype({}, typed_nonempty_list())))].
