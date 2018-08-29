-module(istype_string_test).

-compile(export_all).

-compile({parse_transform, istype_transform}).

-include("istype.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([]).

%%=====================================
%% string()
%%=====================================
%% @doc string() :: [char()]
%% @end
-type any_string() :: string().

is_string_test_() ->
    [?_assertEqual(false, (istype(atom, string()))), ?_assertEqual(false, (istype(<<"binary">>, string()))),
     ?_assertEqual(true, (istype("", string()))), ?_assertEqual(true, (istype([], string()))), ?_assertEqual(true, (istype("string", string()))),
     ?_assertEqual(true, (istype([$s, $t, $r, $i, $n, $g], string()))), ?_assertEqual(false, (istype([-1], string())))].

is_aliased_string_test_() ->
    [?_assertEqual(false, (istype(atom, any_string()))), ?_assertEqual(false, (istype(<<"binary">>, any_string()))),
     ?_assertEqual(true, (istype("", any_string()))), ?_assertEqual(true, (istype([], any_string()))),
     ?_assertEqual(true, (istype("string", any_string()))), ?_assertEqual(true, (istype([$s, $t, $r, $i, $n, $g], any_string()))),
     ?_assertEqual(false, (istype([-1], any_string())))].

to_string_test_() ->
    {setup, fun () -> open_port({fd, 1, 1}, []) end, fun (Port) -> port_close(Port) end,
     fun (Port) ->
	     Pid = self(),
	     Ref = make_ref(),
	     Fun = fun () -> ok end,
	     [?_assertEqual([], (totype([], string()))), ?_assertEqual("string", (totype("string", string()))),
	      ?_assertError({istype_conversion, _, _, _}, (totype([-1], string()))), ?_assertEqual((pid_to_list(Pid)), (totype(Pid, string()))),
	      ?_assertEqual((port_to_list(Port)), (totype(Port, string()))), ?_assertEqual((ref_to_list(Ref)), (totype(Ref, string()))),
	      ?_assertEqual("atom", (totype(atom, string()))), ?_assertEqual("", (totype(<<"">>, string()))),
	      ?_assertEqual("bitstring", (totype(<<"bitstring">>, string()))), ?_assertEqual("binary", (totype(<<"binary">>, string()))),
	      ?_assertEqual("1.10000000000000008882", (totype(1.1, string()))), ?_assertEqual("123456789", (totype(123456789, string()))),
	      ?_assertEqual([], (totype(#{}, string()))), ?_assertError({istype_conversion, _, _, _}, (totype(#{1 => 1}, string()))),
	      ?_assertEqual((erlang:fun_to_list(Fun)), (totype(Fun, string()))), ?_assertEqual([], (totype({}, string()))),
	      ?_assertEqual("string", (totype({$s, $t, $r, $i, $n, $g}, string()))),
	      ?_assertError({istype_conversion, _, _, _}, (totype({-1}, string())))]
     end}.
