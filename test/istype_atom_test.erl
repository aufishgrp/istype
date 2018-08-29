-module(istype_atom_test).

-compile(export_all).

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
-type any_atom() :: atom().

-type literal_atom() :: atom.

is_atom_test_() -> [?_assertEqual(true, (istype(atom, atom()))), ?_assertEqual(false, (istype(<<"binary">>, atom())))].

is_aliased_atom_test_() -> [?_assertEqual(true, (istype(atom, any_atom()))), ?_assertEqual(false, (istype(<<"binary">>, any_atom())))].

is_literal_atom_test_() ->
    [?_assertEqual(true, (istype(atom, atom))), ?_assertEqual(false, (istype(binary, atom))),
     ?_assertEqual(false, (istype(<<"binary">>, atom)))].

to_atom_test_() ->
    [?_assertEqual(atom, (totype(atom, atom()))), ?_assertEqual(atom, (totype(<<"atom">>, atom()))),
     ?_assertEqual(atom, (totype("atom", atom()))), ?_assertError({istype_conversion, _, _, _}, (totype(1, atom())))].

to_aliased_atom_test_() ->
    [?_assertEqual(atom, (totype(atom, any_atom()))), ?_assertEqual(atom, (totype(<<"atom">>, any_atom()))),
     ?_assertEqual(atom, (totype("atom", any_atom()))), ?_assertError({istype_conversion, _, _, _}, (totype(1, any_atom())))].

to_specific_atom_test_() ->
    [?_assertEqual(atom, (totype(atom, literal_atom()))), ?_assertEqual(atom, (totype(<<"atom">>, literal_atom()))),
     ?_assertEqual(atom, (totype("atom", literal_atom()))), ?_assertError({istype_conversion, _, _, _}, (totype(undefined, literal_atom()))),
     ?_assertError({istype_conversion, _, _, _}, (totype(<<"undefined">>, literal_atom()))),
     ?_assertError({istype_conversion, _, _, _}, (totype("undefined", literal_atom())))].
