-module(istype_iolist_test).

-compile({parse_transform, istype_transform}).

-include("istype.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([]).

%%=====================================
%% Atom
%%=====================================
%% @doc Atom :: atom()
%%            | Erlang_Atom
%%
%%      Erlang_Atom :: 'foo', 'bar', ...
%% @end
-type any_iolist() :: iolist().

is_iolist_test_() ->
    [?_assertEqual(true,  istype([$a],                     iolist())),
     ?_assertEqual(true,  istype([<<"binary">>],           iolist())),
     ?_assertEqual(true,  istype([[$a]],                   iolist())),
     ?_assertEqual(true,  istype([[<<"binary">>]],         iolist())),
     ?_assertEqual(true,  istype([$a, <<"binary">>, [$a]], iolist())),
     
     ?_assertEqual(true,  istype([$a | <<"binary">>],                            iolist())),
     ?_assertEqual(true,  istype([<<"binary">> | <<"binary">>],                  iolist())),
     ?_assertEqual(true,  istype([[$a] | <<"binary">>],                          iolist())),
     ?_assertEqual(true,  istype([[<<"binary">>] | <<"binary">>],                iolist())),
     ?_assertEqual(true,  istype([$a, <<"binary">>, [$a] | <<"binary">>],        iolist())),
     ?_assertEqual(true,  istype([[<<"binary">> | <<"binary">>] | <<"binary">>], iolist())),

     ?_assertEqual(false, istype(atom,         iolist())),
     ?_assertEqual(false, istype(<<"binary">>, iolist())),
     ?_assertEqual(false, istype([atom],       iolist()))].

is_aliased_iolist_test_() ->
    [?_assertEqual(true,  istype([$a],                     any_iolist())),
     ?_assertEqual(true,  istype([<<"binary">>],           any_iolist())),
     ?_assertEqual(true,  istype([[$a]],                   any_iolist())),
     ?_assertEqual(true,  istype([[<<"binary">>]],         any_iolist())),
     ?_assertEqual(true,  istype([$a, <<"binary">>, [$a]], any_iolist())),
     
     ?_assertEqual(true,  istype([$a | <<"binary">>],                            any_iolist())),
     ?_assertEqual(true,  istype([<<"binary">> | <<"binary">>],                  any_iolist())),
     ?_assertEqual(true,  istype([[$a] | <<"binary">>],                          any_iolist())),
     ?_assertEqual(true,  istype([[<<"binary">>] | <<"binary">>],                any_iolist())),
     ?_assertEqual(true,  istype([$a, <<"binary">>, [$a] | <<"binary">>],        any_iolist())),
     ?_assertEqual(true,  istype([[<<"binary">> | <<"binary">>] | <<"binary">>], any_iolist())),

     ?_assertEqual(false, istype(atom,         any_iolist())),
     ?_assertEqual(false, istype(<<"binary">>, any_iolist())),
     ?_assertEqual(false, istype([atom],       any_iolist()))].
