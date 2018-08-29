-module(istype_test).

-compile(export_all).

-compile({parse_transform, istype_transform}).

%% API exports
-export([return_value/1, to_src/0]).

-include("istype.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(CONVERT_ERROR(Value, Type),
	fun () ->
		?assertEqual(ok,
			     try totype(Value, Type) catch
			       error:{istype_conversion, _, _, _} -> ok;
			       Err:Or -> io:format("Error: ~p ~p\n~p\n", [Err, Or, erlang:get_stacktrace()]), wrong_error
			     end)
	end()).

%%=============================================================================
%% Test records
%%=============================================================================
-record(record_a, {a = atom  :: atom(), b = atom, c  :: atom(), d, e  :: atom() | binary()}).

-record(record_b, {f  :: #record_a{e :: binary()}, g  :: atom() | integer()}).

-record(record_c, {a = "atom"  :: list(), e  :: atom() | binary()}).

-export_type([any_type/0, none_type/0, pid_type/0, port_type/0, reference_type/0, nil_a_type/0, nil_b_type/0, empty_bitstring_type/0,
	      m_bitstring_type/0, n_bitstring_type/0, mn_bitstring_type/0, float_type/0, any_fun_type/0, any_arity_fun_returning_type/0,
	      fun_returning_type/0, typed_fun_type/0, integer_type/0, erlang_integer_type/0, erlang_negative_integer_type/0, range_type/0,
	      neg_range_type/0, typed_list_type/0, maybe_improper_list_type/0, nonempty_improper_list_type/0, nonempty_list_type/0, any_map_type/0,
	      empty_map_type/0, mandatory_map_type/0, optional_map_type/0, mixed_map_type/0, union_a_type/0, union_b_type/0, union_c_type/0, term_type/0,
	      binary_type/0, bitstring_type/0, boolean_type/0, byte_type/0, char_type/0, number_type/0, string_type/0, nonempty_string_type/0,
	      module_type/0, mfa_type/0, arity_type/0, identifier_type/0, node_type/0, timeout_type/0, no_return_type/0, non_neg_integer_type/0,
	      pos_integer_type/0, neg_integer_type/0, record_a_type/0, typed_record_a_type/0, record_b_type/0, alias_maybe_improper_list_type/0,
	      iodata_type/0, iolist_type/0, function_type/0, nonempty_maybe_improper_list_type/0, typed_nonempty_maybe_improper_list_type/0,
	      typed_record_b_type/0]).

%%=============================================================================
%% General test functions
%%=============================================================================
%% @doc Returns the value.
%%      Used to prevent compiler warnings that a match can never succeed
%%      as the value type would be known at compile time.
%% @end
return_value(Value) -> Value.

-type remote_2() :: istype_remote:type_2().

-type remote_1() :: istype_remote:type_1(atom()).

-type remote_0() :: istype_remote:type_0(atom(), binary()).

remote_test() -> ?assertEqual(true, (istype(apple, istype_remote:type_a()))).

%% @doc Assure that the assertion transform works as expected.
assert_test() ->
    TestBinary = return_value(<<"binary">>),
    ?assertEqual(true, (asserttype(atom, atom()))),
    ?assertEqual(true, try asserttype(TestBinary, atom()), false catch error:{badmatch, false} -> true end).

invocation_test() ->
    TestAtom = return_value(atom),
    TestRecord = #record_a{},
    ?assertEqual(true, (istype(TestRecord#record_a.a, atom()))),
    ?assertEqual(true, (istype(atom, atom()))),
    ?assertEqual(true, (istype(TestAtom, atom()))),
    ?assertEqual(true, (istype(invocation_test_1(), atom()))),
    ?assertEqual(true, (istype(return_value(atom), atom()))),
    ?assertEqual(true, (istype((?MODULE):return_value(atom), atom()))),
    ?assertEqual(true, (istype([X1 || X1 <- lists:seq(0, 1)], list()))),
    ?assertEqual(true, (istype(case atom of X0 -> X0 end, atom()))),
    ?assertEqual(true, (istype(begin return_value(atom) end, atom()))),
    ?assertEqual(true, (istype(try 1 = 1 catch _:_ -> undefined end, integer()))).

invocation_test_1() -> atom.

guard_test() ->
    TestAtom = return_value(atom),
    ?assertEqual(true, case (?MODULE):return_value(atom) of X0 when istype(X0, atom()) -> true end),
    Record1 = return_value(#record_a{}),
    ?assertEqual(true, case Record1 of X1 when istype(X1, #record_a{}) -> true end),
    ?assertEqual(true, (guard_test_1(TestAtom))).

guard_test_1(X2) when istype(X2, atom()) -> true;
guard_test_1(_) -> false.

%%=============================================================================
%% Type test functions
%%=============================================================================
%% Predefined types
%%=========================================================
%% any()
%%=====================================
-type any_type() :: any().

any_validation_test() ->
    ?assertEqual(true, (istype(return_value(atom), any()))),
    ?assertEqual(true, (istype(return_value(<<"binary">>), any()))),
    ?assertEqual(true, (istype(return_value(1.0), any()))),
    ?assertEqual(true, (istype(return_value(1), any()))),
    ?assertEqual(true, (istype(return_value("list"), any()))).

any_conversion_test() ->
    ?assertEqual(atom, (totype(atom, any()))),
    ?assertEqual(<<"binary">>, (totype(<<"binary">>, any()))),
    ?assertEqual(1.0, (totype(1.0, any()))),
    ?assertEqual(1, (totype(1, any()))),
    ?assertEqual("list", (totype("list", any()))).

%%=====================================
%% none()
%%=====================================
-type none_type() :: none().

none_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), none()))),
    ?assertEqual(false, (istype(return_value(<<"binary">>), none()))),
    ?assertEqual(false, (istype(return_value(1.0), none()))),
    ?assertEqual(false, (istype(return_value(1), none()))),
    ?assertEqual(false, (istype(return_value("list"), none()))).

none_conversion_test() ->
    ?CONVERT_ERROR(atom, (none())),
    ?CONVERT_ERROR(<<"binary">>, (none())),
    ?CONVERT_ERROR(1.0, (none())),
    ?CONVERT_ERROR(1, (none())),
    ?CONVERT_ERROR("list", (none())).

%%=====================================
%% pid()
%%=====================================
-type pid_type() :: pid().

pid_validation_test() ->
    Self = self(), ?assertEqual(false, (istype(return_value(atom), pid()))), ?assertEqual(true, (istype(return_value(Self), pid()))).

pid_conversion_test() ->
    Self = self(),
    ListSelf = pid_to_list(Self),
    BinarySelf = list_to_binary(ListSelf),
    Self = totype(BinarySelf, pid()),
    Self = totype(ListSelf, pid()),
    Self = totype(Self, pid()),
    ?CONVERT_ERROR(atom, (pid())).

%%=====================================
%% port()
%%=====================================
-type port_type() :: port().

port_validation_test() ->
    Port = open_port({spawn, "tar -xzf -"}, [exit_status, binary]),
    ?assertEqual(false, (istype(return_value(atom), port()))),
    ?assertEqual(true, (istype(return_value(Port), port()))),
    ?assertEqual(true, (port_close(Port))).

port_conversion_test() ->
    Port = open_port({spawn, "tar -xzf -"}, [exit_status, binary]),
    ListPort = port_to_list(Port),
    BinaryPort = list_to_binary(ListPort),
    Port = totype(BinaryPort, port()),
    Port = totype(ListPort, port()),
    Port = totype(Port, port()),
    ?CONVERT_ERROR(atom, (port())),
    ?assertEqual(true, (port_close(Port))).

%%=====================================
%% reference()
%%=====================================
-type reference_type() :: reference().

reference_validation_test() ->
    Ref = make_ref(),
    ?assertEqual(false, (istype(return_value(atom), reference()))),
    ?assertEqual(true, (istype(return_value(Ref), reference()))).

reference_conversion_test() ->
    Ref = make_ref(),
    ListRef = ref_to_list(Ref),
    BinaryRef = list_to_binary(ListRef),
    Ref = totype(BinaryRef, reference()),
    Ref = totype(ListRef, reference()),
    Ref = totype(Ref, reference()).

%%=====================================
%% [] - nil()
%%=====================================
-type nil_a_type() :: [].

-type nil_b_type() :: [].

nil_valdiation_test() ->
    ?assertEqual(false, (istype(return_value(atom), []))),
    ?assertEqual(false, (istype(return_value(atom), nil()))),
    ?assertEqual(false, (istype(return_value(atom), nil_a_type()))),
    true = istype(return_value([]), []),
    true = istype(return_value([]), nil()),
    true = istype(return_value([]), nil_a_type()).

nil_conversion_test() ->
    [] = totype([], []),
    [] = totype([], nil()),
    [] = totype([], nil_a_type()),
    ?CONVERT_ERROR(atom, []),
    ?CONVERT_ERROR(atom, (nil())),
    ?CONVERT_ERROR(atom, (nil_a_type())).

%%=====================================
%% Bitstring
%%=====================================
%% @doc Bitstring :: <<>>
%%                 | <<_:M>>
%%                 | <<_:_*N>>
%%                 | <<_:M, _:_*N>>
%%
%%      M :: integer() >= 1
%%      N :: integer() >= 1
%% @end
-type empty_bitstring_type() :: <<>>.

-type m_bitstring_type() :: <<_:1>>.

-type n_bitstring_type() :: <<_:_*4>>.

-type mn_bitstring_type() :: <<_:1, _:_*4>>.

literal_bitstring_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), <<>>))),
    ?assertEqual(true, (istype(return_value(<<>>), <<>>))),
    ?assertEqual(true, (istype(return_value(<<>>), empty_bitstring_type()))),
    ?assertEqual(false, (istype(return_value(atom), m_bitstring_type()))),
    ?assertEqual(true, (istype(return_value(<<0:1>>), m_bitstring_type()))),
    ?assertEqual(false, (istype(return_value(atom), n_bitstring_type()))),
    ?assertEqual(false, (istype(return_value(<<0:1>>), n_bitstring_type()))),
    ?assertEqual(true, (istype(return_value(<<0:4>>), n_bitstring_type()))),
    ?assertEqual(true, (istype(return_value(<<0:8>>), n_bitstring_type()))),
    ?assertEqual(false, (istype(return_value(atom), mn_bitstring_type()))),
    ?assertEqual(true, (istype(return_value(<<0:1>>), mn_bitstring_type()))),
    ?assertEqual(false, (istype(return_value(<<0:4>>), mn_bitstring_type()))),
    ?assertEqual(true, (istype(return_value(<<0:5>>), mn_bitstring_type()))).

literal_bitstring_conversion_test() ->
    <<>> = totype(<<>>, empty_bitstring_type()),
    <<>> = totype([], empty_bitstring_type()),
    ?CONVERT_ERROR([1], (m_bitstring_type())),
    ?CONVERT_ERROR((#{a => 1}), (m_bitstring_type())),
    ?CONVERT_ERROR(<<0:0>>, (m_bitstring_type())),
    ?CONVERT_ERROR(<<0:1>>, (n_bitstring_type())),
    ?CONVERT_ERROR(<<0:4>>, (mn_bitstring_type())).

%%=====================================
%% float()
%%=====================================
-type float_type() :: float().

float_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), float()))),
    ?assertEqual(true, (istype(return_value(1.0), float()))),
    ?assertEqual(false, (istype(return_value(1), float()))).

float_conversion_test() ->
    ?assertEqual(1.0, (totype(<<"1">>, float()))),
    ?assertEqual(1.0, (totype(<<"1.0">>, float()))),
    ?assertEqual(1.0, (totype(<<"1.00000000e+00">>, float()))),
    ?assertEqual(1.0, (totype(1.0, float()))),
    ?assertEqual(1.0, (totype(1, float()))),
    ?assertEqual(1.0, (totype("1", float()))),
    ?assertEqual(1.0, (totype("1.0", float()))),
    ?assertEqual(1.0, (totype("1.00000000e+00", float()))),
    ?assertEqual((-1.0), (totype(<<"-1">>, float()))),
    ?assertEqual((-1.0), (totype(<<"-1.0">>, float()))),
    ?assertEqual((-1.0), (totype(<<"-1.00000000e+00">>, float()))),
    ?assertEqual((-1.0), (totype(-1.0, float()))),
    ?assertEqual((-1.0), (totype(-1, float()))),
    ?assertEqual((-1.0), (totype("-1", float()))),
    ?assertEqual((-1.0), (totype("-1.0", float()))),
    ?assertEqual((-1.0), (totype("-1.00000000e+00", float()))),
    ?CONVERT_ERROR(atom, (float())).

%%=====================================
%% Fun
%%=====================================
%% @doc Fun :: fun()                %% Any fun
%%           | fun((...) -> Type)   %% Any arity returning Type
%%           | fun(() -> Type)
%%           | fun((TList) -> Type)
%%
%%      TList :: Type
%%             | Type, TList
%% @end
-type any_fun_type() :: fun().

-type any_arity_fun_returning_type() :: fun((...) -> atom()).

-type fun_returning_type() :: fun(() -> atom()).

-type typed_fun_type() :: fun((atom()) -> atom()).

fun_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), 'fun'()))),
    ?assertEqual(true, (istype(return_value(fun return_value/1), 'fun'()))),
    ?assertEqual(true, (istype(return_value(fun () -> ok end), 'fun'()))).

fun_conversion_test() ->
    Fun = fun () -> ok end,
    ?CONVERT_ERROR(atom, ('fun'())),
    ?CONVERT_ERROR(Fun, ('fun'())),
    ?CONVERT_ERROR(Fun, (any_arity_fun_returning_type())),
    ?CONVERT_ERROR(Fun, (fun_returning_type())),
    ?CONVERT_ERROR(Fun, (typed_fun_type())).

%%=====================================
%% Integer
%%=====================================
%% @doc Integer :: integer()
%%               | Erlang_Integer
%%               | Erlang_Integer..Erlang_Integer %% range()
%%
%%      Erlang_Integer :: ..., -1, 0, 1, ..., 42, ...
%% @end
%%=================
%% integer()
%%=================
-type integer_type() :: integer().

integer_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), integer()))),
    ?assertEqual(false, (istype(return_value(1.0), integer()))),
    ?assertEqual(true, (istype(return_value(1), integer()))),
    ?assertEqual(true, (istype(return_value(-1), integer()))).

integer_conversion_test() ->
    ?assertEqual(1, (totype(1, integer()))),
    ?assertEqual(1, (totype(<<"1">>, integer()))),
    ?assertEqual(1, (totype(<<"1.0">>, integer()))),
    ?assertEqual(1, (totype(<<"1.00000000e+00">>, integer()))),
    ?assertEqual(1, (totype("1", integer()))),
    ?assertEqual(1, (totype("1.0", integer()))),
    ?assertEqual(1, (totype("1.00000000e+00", integer()))),
    ?assertEqual((-1), (totype(<<"-1">>, integer()))),
    ?CONVERT_ERROR(atom, (integer())).

%%=================
%% Erlang_Integer
%%=================
-type erlang_integer_type() :: 1.

-type erlang_negative_integer_type() :: -1.

erlang_integer_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), 1))),
    ?assertEqual(false, (istype(return_value(1.0), 1))),
    ?assertEqual(true, (istype(return_value(1), 1))),
    ?assertEqual(true, (istype(return_value(-1), -1))).

erlang_integer_conversion_test() ->
    ?assertEqual(1, (totype(<<"1">>, 1))),
    ?assertEqual(1, (totype("1", 1))),
    ?assertEqual(1, (totype(1, 1))),
    ?assertEqual(1, (totype(<<"1.0">>, 1))),
    ?assertEqual(1, (totype("1.0", 1))),
    ?assertEqual(1, (totype(1.0, 1))),
    ?CONVERT_ERROR(<<"2.1">>, 1), %% 2.1 as 1.1 would convert to 1 on float to int.
    ?CONVERT_ERROR("2.1", 1),
    ?CONVERT_ERROR((-1), 1),
    ?CONVERT_ERROR(atom, 1).

%%================
%% Erlang_Integer..Erlang_Integer
%%=================
-type range_type() :: 1..100.

-type neg_range_type() :: -1..1.

range_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), range_type()))),
    ?assertEqual(false, (istype(return_value(-1), range_type()))),
    ?assertEqual(true, (istype(return_value(1), range_type()))),
    ?assertEqual(false, (istype(return_value(1.0), range_type()))),
    ?assertEqual(false, (istype(return_value(101), range_type()))),
    ?assertEqual(false, (istype(-2, neg_range_type()))),
    ?assertEqual(true, (istype(0, neg_range_type()))),
    ?assertEqual(false, (istype(2, neg_range_type()))).

range_conversion_test() ->
    ?assertEqual(1, (totype(<<"1">>, range_type()))),
    ?assertEqual(1, (totype(1.0, range_type()))),
    ?assertEqual(1, (totype(1, range_type()))),
    ?assertEqual(1, (totype("1", range_type()))),
    ?CONVERT_ERROR(atom, (range_type())).

%%=====================================
%% List
%%=====================================
%% @doc List :: list(Type)
%%            | maybe_improper_list(Type1, Type2)
%%            | nonempty_improper_list(Type1, Type2)
%%            | nonempty_list(Type)
%%
%% @end
-type typed_list_type() :: [atom()].

-type maybe_improper_list_type() :: maybe_improper_list(atom(), atom() | []).

-type nonempty_improper_list_type() :: nonempty_improper_list(atom(), atom()).

-type nonempty_list_type() :: [atom(), ...].

list_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), list(any())))),
    ?assertEqual(true, (istype(return_value([atom]), list(any())))),
    ?assertEqual(true, (istype(return_value([]), list(atom())))),
    ?assertEqual(false, (istype(return_value([1]), list(atom())))),
    ?assertEqual(true, (istype(return_value([atom]), list(atom())))),
    ?assertEqual(true, (istype(return_value([atom, atom]), list(atom())))),
    ?assertEqual(false, (istype(return_value([atom, 1]), list(atom())))),
    ?assertEqual(true, (istype(return_value([]), typed_list_type()))),
    ?assertEqual(false, (istype(return_value([1]), typed_list_type()))),
    ?assertEqual(true, (istype(return_value([atom]), typed_list_type()))),
    ?assertEqual(true, (istype(return_value([atom, atom]), typed_list_type()))),
    ?assertEqual(false, (istype(return_value([atom, 1]), typed_list_type()))),
    ?assertEqual(true, (istype(return_value([]), maybe_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([1]), maybe_improper_list_type()))),
    ?assertEqual(true, (istype(return_value([atom]), maybe_improper_list_type()))),
    ?assertEqual(true, (istype(return_value([atom | atom]), maybe_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([atom | 1]), maybe_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([]), nonempty_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([1]), nonempty_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([atom]), nonempty_improper_list_type()))),
    ?assertEqual(true, (istype(return_value([atom | atom]), nonempty_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([atom | 1]), nonempty_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([]), nonempty_list_type()))),
    ?assertEqual(false, (istype(return_value([1]), nonempty_list_type()))),
    ?assertEqual(true, (istype(return_value([atom]), nonempty_list_type()))),
    ?assertEqual(true, (istype(return_value([atom, atom]), nonempty_list_type()))),
    ?assertEqual(false, (istype(return_value([atom, 1]), nonempty_list_type()))).

list_conversion_test() ->
    [] = totype([], typed_list_type()),
    [atom, atom] = totype([atom, "atom"], typed_list_type()),
    ?CONVERT_ERROR([atom, 1], (typed_list_type())),
    [] = totype([], maybe_improper_list_type()),
    [atom, atom] = totype([atom, "atom"], maybe_improper_list_type()),
    [atom | atom] = totype([atom | <<"atom">>], maybe_improper_list_type()),
    ?CONVERT_ERROR([atom, 1], (maybe_improper_list_type())),
    ?CONVERT_ERROR([], (nonempty_improper_list_type())),
    [atom | atom] = totype([atom | <<"atom">>], nonempty_improper_list_type()),
    ?CONVERT_ERROR([atom, 1], (nonempty_improper_list_type())),
    ?CONVERT_ERROR([], (nonempty_list_type())),
    [atom, atom] = totype([atom, "atom"], nonempty_list_type()),
    ?CONVERT_ERROR([atom, 1], (nonempty_list_type())).

%%=====================================
%% Map
%%=====================================
%% @doc Map :: map() %% Any map
%%           | #{}   %% Empty map
%%           | #{PairList}
%%
%%      PairList :: Pair
%%                | Pair, PairList
%%
%%      Pair :: Type := Type %% Mandatory pair
%%            | Type => Type %% Optional pair
%% @end
-type any_map_type() :: map().

-type empty_map_type() :: #{}.

-type mandatory_map_type() :: #{atom() := atom()}.

-type mandatory_map_key_type() :: #{atom := atom()}.

-type mandatory_map_value_type() :: #{atom() := atom}.

-type optional_map_type() :: #{binary() => binary()}.

-type optional_map_key_type() :: #{atom => binary(), binary() => binary()}.

-type optional_map_value_type() :: #{binary() => atom, binary() => binary()}.

-type mixed_map_type() :: #{atom := atom(), binary => binary()}.

any_map_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), map()))), ?assertEqual(true, (istype(return_value(#{}), map()))).

any_map_conversion_test() ->
    #{} = totype([], map()),
    #{} = totype(#{}, map()),
    #{a := b, b := c} = totype([{a, b}, {b, c}], map()),
    #{a := b, b := c} = totype(#{a => b, b => c}, map()),
    #{a := atom, b := atom, c := undefined, d := undefined, e := undefined} = totype(#record_a{}, map()),
    ?CONVERT_ERROR(atom, (map())).

empty_map_validation_test() -> ?assertEqual(false, (istype(return_value(atom), #{}))), ?assertEqual(true, (istype(return_value(#{}), #{}))).

empty_map_conversion_test() -> #{} = totype([], #{}), #{} = totype(#{}, #{}), ?CONVERT_ERROR(atom, (#{})).

mandatory_map_validation_test() ->
    ?assertEqual(false, (istype(return_value(#{}), mandatory_map_type()))),
    ?assertEqual(false, (istype(return_value(#{1 => 1}), mandatory_map_type()))),
    ?assertEqual(false, (istype(return_value(#{atom => 1}), mandatory_map_type()))),
    ?assertEqual(true, (istype(return_value(#{atom => atom}), mandatory_map_type()))).

mandatory_map_conversion_test() ->
    ?CONVERT_ERROR((#{}), (mandatory_map_type())),
    ?CONVERT_ERROR([], (mandatory_map_type())),
    %% Type conversion unnecessary
    #{atom := atom} = totype(#{atom => atom}, mandatory_map_type()),
    #{atom := atom} = totype([{atom, atom}], mandatory_map_type()),
    %% Key type is literal
    #{atom := atom} = totype(#{atom => atom}, mandatory_map_key_type()),
    #{atom := atom} = totype(#{<<"atom">> => <<"atom">>}, mandatory_map_key_type()),
    #{atom := atom} = totype(#{"atom" => "atom"}, mandatory_map_key_type()),
    #{atom := atom} = totype(#{atom => <<"atom">>}, mandatory_map_key_type()),
    #{atom := atom} = totype(#{atom => "atom"}, mandatory_map_key_type()),
    %% Value type is literal
    #{atom := atom} = totype(#{atom => atom}, mandatory_map_value_type()),
    #{atom := atom} = totype(#{<<"atom">> => <<"atom">>}, mandatory_map_value_type()),
    #{atom := atom} = totype(#{"atom" => "atom"}, mandatory_map_value_type()),
    #{atom := atom} = totype(#{atom => <<"atom">>}, mandatory_map_value_type()),
    #{atom := atom} = totype(#{atom => "atom"}, mandatory_map_value_type()),
    ?CONVERT_ERROR(atom, (mandatory_map_type())).

optional_map_validation_test() ->
    ?assertEqual(true, (istype(return_value(#{}), optional_map_type()))),
    ?assertEqual(true, (istype(return_value(#{1 => 1}), optional_map_type()))),
    ?assertEqual(true, (istype(return_value(#{atom => <<"binary">>}), optional_map_type()))),
    ?assertEqual(true, (istype(return_value(#{<<"binary">> => <<"binary">>}), optional_map_type()))).

optional_map_conversion_test() ->
    ?assertEqual((#{}), (totype(return_value(#{}), optional_map_type()))),
    ?assertEqual((#{}), (totype(return_value([]), optional_map_type()))),
    %% Type conversion unnecessary
    ?assertEqual((#{<<"binary">> => <<"binary">>}), (totype(return_value(#{<<"binary">> => <<"binary">>}), optional_map_type()))),
    ?assertEqual((#{<<"binary">> => <<"binary">>}), (totype(return_value([{<<"binary">>, <<"binary">>}]), optional_map_type()))),
    %% No types match optional specs
    ?assertEqual((#{"binary" => "binary"}), (totype(return_value(#{"binary" => "binary"}), optional_map_type()))),
    %% Key type is literal
    ?assertEqual((#{atom => <<"binary">>, <<"binary">> => 1}),
		 (totype(return_value(#{<<"atom">> => <<"binary">>, <<"binary">> => 1}), optional_map_key_type()))),
    ?assertEqual((#{atom => <<"binary">>, <<"binary">> => 1}),
		 (totype(return_value(#{"atom" => "binary", <<"binary">> => 1}), optional_map_key_type()))),
    ?assertEqual((#{atom => <<"binary">>, <<"binary">> => 1}),
		 (totype(return_value(#{atom => binary, <<"binary">> => 1}), optional_map_key_type()))),
    %% Key value is literal
    ?assertEqual((#{<<"binary">> => atom, <<"binary">> => 1}),
		 (totype(return_value(#{<<"binary">> => <<"atom">>, <<"binary">> => 1}), optional_map_value_type()))),
    ?assertEqual((#{<<"binary">> => atom, <<"binary">> => 1}),
		 (totype(return_value(#{"binary" => "atom", <<"binary">> => 1}), optional_map_value_type()))),
    ?assertEqual((#{<<"binary">> => atom, <<"binary">> => 1}),
		 (totype(return_value(#{binary => atom, <<"binary">> => 1}), optional_map_value_type()))),
    ?CONVERT_ERROR(atom, (optional_map_type())).

mixed_map_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), mixed_map_type()))),
    ?assertEqual(false, (istype(return_value(#{}), mixed_map_type()))),
    ?assertEqual(true, (istype(return_value(#{atom => atom}), mixed_map_type()))),
    ?assertEqual(true, (istype(return_value(#{atom => atom, binary => <<"binary">>}), mixed_map_type()))),
    ?assertEqual(false, (istype(return_value(#{binary => <<"binary">>}), mixed_map_type()))),
    ?assertEqual(false, (istype(return_value(#{atom => atom, binary => <<"binary">>, 1 => 1}), mixed_map_type()))).

mixed_map_conversion_test() ->
    #{atom := atom} = totype(return_value(#{<<"atom">> => atom}), mixed_map_type()),
    #{atom := atom} = totype(return_value([{atom, atom}]), mixed_map_type()),
    #{atom := atom, binary := <<"binary">>} = totype(return_value(#{atom => atom, <<"binary">> => <<"binary">>}), mixed_map_type()),
    #{atom := atom, binary := <<"binary">>} = totype(return_value([{atom, atom}, {<<"binary">>, <<"binary">>}]), mixed_map_type()),
    ?CONVERT_ERROR([{<<"binary">>, binary}], (mixed_map_type())).

%%=====================================
%% Union
%%=====================================
%% @doc Union :: Type1 | Type2
%% @end
-type union_a_type() :: atom() | integer().

-type union_b_type() :: integer() | atom().

-type union_c_type() :: atom | <<_:_*1>>.

union_validation_test() ->
    ?assertEqual(true, (istype(return_value(atom), union_a_type()))),
    ?assertEqual(true, (istype(return_value(also), union_a_type()))),
    ?assertEqual(true, (istype(return_value(0), union_a_type()))),
    ?assertEqual(true, (istype(return_value(1), union_a_type()))),
    ?assertEqual(false, (istype(return_value(<<"binary">>), union_a_type()))),
    ?assertEqual(true, (istype(return_value(atom), union_b_type()))),
    ?assertEqual(true, (istype(return_value(also), union_b_type()))),
    ?assertEqual(true, (istype(return_value(0), union_b_type()))),
    ?assertEqual(true, (istype(return_value(1), union_b_type()))),
    ?assertEqual(false, (istype(return_value(<<"binary">>), union_b_type()))),
    ?assertEqual(true, (istype(return_value(atom), union_c_type()))),
    ?assertEqual(false, (istype(return_value(also), union_c_type()))),
    ?assertEqual(false, (istype(return_value(0), union_c_type()))),
    ?assertEqual(true, (istype(return_value(<<"binary">>), union_c_type()))),
    ?assertEqual(true, (istype(return_value(<<"binary also">>), union_c_type()))).

union_conversion_test() ->
    ?assertEqual(atom, (totype(return_value(atom), union_a_type()))),
    ?assertEqual(atom, (totype(return_value(<<"atom">>), union_a_type()))),
    also = totype(return_value("also"), union_a_type()),
    '1.0' = totype(return_value(<<"1.0">>), union_a_type()),
    '1' = totype(return_value("1"), union_a_type()),
    ?CONVERT_ERROR({}, (union_a_type())),
    ?assertEqual(atom, (totype(return_value(atom), union_b_type()))),
    ?assertEqual(atom, (totype(return_value(<<"atom">>), union_b_type()))),
    also = totype(return_value("also"), union_b_type()),
    ?assertEqual(1, (totype(return_value(<<"1.0">>), union_b_type()))),
    ?assertEqual(1, (totype(return_value("1"), union_b_type()))),
    ?CONVERT_ERROR({}, (union_b_type())),
    ?assertEqual(atom, (totype(return_value(atom), union_c_type()))),
    ?assertEqual(atom, (totype(return_value(<<"atom">>), union_c_type()))),
    <<"also">> = totype(return_value("also"), union_c_type()),
    <<"1.0">> = totype(return_value(<<"1.0">>), union_c_type()),
    <<"1">> = totype(return_value("1"), union_c_type()),
    ?CONVERT_ERROR({}, (union_c_type())).

%%=========================================================
%% Predefined Aliases
%%=========================================================
%% term()
%%=====================================
%% @doc term() :: any()
%% @end
-type term_type() :: term().

term_validation_test() ->
    ?assertEqual(true, (istype(return_value(atom), term()))),
    ?assertEqual(true, (istype(return_value(<<"binary">>), term()))),
    ?assertEqual(true, (istype(return_value(1.0), term()))),
    ?assertEqual(true, (istype(return_value(1), term()))),
    ?assertEqual(true, (istype(return_value("list"), term()))).

term_conversion_test() ->
    ?assertEqual(atom, (totype(atom, term()))),
    <<"binary">> = totype(<<"binary">>, term()),
    ?assertEqual(1.0, (totype(1.0, term()))),
    ?assertEqual(1, (totype(1, term()))),
    "list" = totype("list", term()).

%%=====================================
%% binary()
%%=====================================
%% @doc binary() :: <<_:_*8>>
%% @end
-type binary_type() :: binary().

binary_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), binary()))), ?assertEqual(true, (istype(return_value(<<"binary">>), binary()))).

binary_conversion_test() ->
    <<"binary">> = totype(binary, binary()),
    <<"binary">> = totype(<<"binary">>, binary()),
    <<"1.0">> = totype(1.0, binary()),
    <<"1">> = totype(1, binary()),
    <<"binary">> = totype("binary", binary()),
    <<"#Fun<istype", _/binary>> = totype(fun () -> atom end, binary()),
    Ref = make_ref(),
    BinaryRef = list_to_binary(ref_to_list(Ref)),
    BinaryRef = totype(Ref, binary()),
    Pid = self(),
    BinaryPid = list_to_binary(pid_to_list(Pid)),
    BinaryPid = totype(Pid, binary()).

%%=====================================
%% bitstring()
%%=====================================
%% @doc bitstring() :: <<_:_*1>>
%% @end
-type bitstring_type() :: bitstring().

bitstring_validation_test() -> ?assertEqual(false, (istype(atom, bitstring()))), ?assertEqual(true, (istype(<<"binary">>, bitstring()))).

bitstring_conversion_test() -> <<"bitstring">> = totype(<<"bitstring">>, bitstring()), <<"bitstring">> = totype("bitstring", bitstring()).

%%=====================================
%% boolean()
%%=====================================
%% @doc boolean() :: 'false' | 'true'
%% @end
-type boolean_type() :: boolean().

boolean_validation_test() ->
    ?assertEqual(true, (istype(return_value(true), boolean()))),
    ?assertEqual(true, (istype(return_value(false), boolean()))),
    ?assertEqual(false, (istype(return_value(undefined), boolean()))),
    ?assertEqual(false, (istype(return_value(<<"binary">>), boolean()))).

boolean_conversion_test() ->
    ?assertEqual(true, (totype(true, boolean()))),
    ?assertEqual(true, (totype(<<"true">>, boolean()))),
    ?assertEqual(true, (totype("true", boolean()))).

%%=====================================
%% byte()
%%=====================================
%% @doc byte() :: 0..255
%% @end
-type byte_type() :: byte().

byte_validation_test() ->
    ?assertEqual(false, (istype(atom, byte()))),
    ?assertEqual(true, (istype(0, byte()))),
    ?assertEqual(false, (istype(-1, byte()))),
    ?assertEqual(false, (istype(256, byte()))).

byte_conversion_test() -> 0 = totype(0, byte()), ?CONVERT_ERROR(atom, (byte())), ?CONVERT_ERROR((-1), (byte())).

%%=====================================
%% char()
%%=====================================
%% @doc char() :: 0..16#10ffff
%% @end
-type char_type() :: char().

char_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), char()))),
    ?assertEqual(true, (istype(return_value(0), char()))),
    ?assertEqual(false, (istype(return_value(-1), char()))),
    ?assertEqual(false, (istype(return_value(1114112), char()))).

char_conversion_test() ->
    0 = totype("0", char()),
    0 = totype(<<"0">>, char()),
    0 = totype(0.0, char()),
    ?CONVERT_ERROR(atom, (char())),
    ?CONVERT_ERROR((-1), (char())).

%%=====================================
%% nil()
%%=====================================
%% @doc nil() :: []
%% @end
%% @doc See [] - nil() above
%% @end

%%=====================================
%% number()
%%=====================================
%% @doc number() :: integer() | float()
%% @end
-type number_type() :: number().

number_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), number()))),
    ?assertEqual(true, (istype(return_value(1.0), number()))),
    ?assertEqual(true, (istype(return_value(1), number()))).

number_conversion_test() ->
    ?assertEqual(1.0, (totype(<<"1.0">>, number()))),
    ?assertEqual(1.0, (totype(1.0, number()))),
    ?assertEqual(1, (totype(1, number()))),
    ?assertEqual(1, (totype("1", number()))),
    ?CONVERT_ERROR(atom, (number())).

%%=====================================
%% maybe_improper_list()
%%=====================================
%% @doc maybe_improper_list() :: maybe_improper_list(any(), any())
%% @end
-type alias_maybe_improper_list_type() :: maybe_improper_list().

maybe_improper_list_validation_test() ->
    ?assertEqual(true, (istype(return_value([]), maybe_improper_list()))),
    ?assertEqual(true, (istype(return_value([]), alias_maybe_improper_list_type()))),
    ?assertEqual(true, (istype(return_value([atom | atom]), maybe_improper_list()))),
    ?assertEqual(true, (istype(return_value([atom, atom]), maybe_improper_list()))),
    ?assertEqual(false, (istype(return_value(atom), maybe_improper_list()))).

maybe_improper_list_conversion_test() ->
    [] = totype(return_value({}), maybe_improper_list()), [atom] = totype(return_value({atom}), maybe_improper_list()).

%%=====================================
%% string()
%%=====================================
%% @doc string() :: [char()]
%% @end
-type string_type() :: string().

string_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), string()))),
    ?assertEqual(false, (istype(return_value(<<"binary">>), string()))),
    ?assertEqual(true, (istype(return_value(""), string()))),
    ?assertEqual(true, (istype(return_value([]), string()))),
    ?assertEqual(true, (istype(return_value("string"), string()))),
    ?assertEqual(true, (istype(return_value([$s, $t, $r, $i, $n, $g]), string()))),
    ?assertEqual(false, (istype(return_value([-1]), string()))).

string_conversion_test() ->
    "" = totype(<<"">>, string()),
    "" = totype("", string()),
    "" = totype([], string()),
    "atom" = totype(atom, string()),
    "binary" = totype(<<"binary">>, string()),
    "1.0" = totype(1.0, string()),
    "1" = totype(1, string()),
    "string" = totype("string", string()),
    [$s, $t, $r, $i, $n, $g] = totype([$s, $t, $r, $i, $n, $g], string()),
    Self = self(),
    ListSelf = pid_to_list(Self),
    ListSelf = totype(Self, string()),
    Port = open_port({spawn, "tar -xzf -"}, [exit_status, binary]),
    ListPort = port_to_list(Port),
    ListPort = totype(Port, string()),
    ?assertEqual(true, (port_close(Port))),
    Ref = make_ref(),
    ListRef = ref_to_list(Ref),
    ListRef = totype(Ref, string()),
    ?CONVERT_ERROR([-1], (string())).

%%=====================================
%% nonempty_string()
%%=====================================
%% @doc nonempty_string() :: [char(), ...]
%% @end
-type nonempty_string_type() :: nonempty_string().

nonempty_string_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), nonempty_string()))),
    ?assertEqual(false, (istype(return_value(<<"binary">>), nonempty_string()))),
    ?assertEqual(false, (istype(return_value(""), nonempty_string()))),
    ?assertEqual(false, (istype(return_value([]), nonempty_string()))),
    ?assertEqual(true, (istype(return_value("nonempty_string"), nonempty_string()))),
    ?assertEqual(true, (istype(return_value([$s, $t, $r, $i, $n, $g]), nonempty_string()))),
    ?assertEqual(false, (istype(return_value([-1]), nonempty_string()))).

nonempty_string_conversion_test() ->
    "atom" = totype(atom, nonempty_string()),
    "binary" = totype(<<"binary">>, nonempty_string()),
    "1.0" = totype(1.0, nonempty_string()),
    "1" = totype(1, nonempty_string()),
    "nonempty_string" = totype("nonempty_string", nonempty_string()),
    [$s, $t, $r, $i, $n, $g] = totype([$s, $t, $r, $i, $n, $g], nonempty_string()),
    Self = self(),
    ListSelf = pid_to_list(Self),
    ListSelf = totype(Self, nonempty_string()),
    Port = open_port({spawn, "tar -xzf -"}, [exit_status, binary]),
    ListPort = port_to_list(Port),
    ListPort = totype(Port, nonempty_string()),
    ?assertEqual(true, (port_close(Port))),
    Ref = make_ref(),
    ListRef = ref_to_list(Ref),
    ListRef = totype(Ref, nonempty_string()),
    ?CONVERT_ERROR([-1], (nonempty_string())),
    ?CONVERT_ERROR("", (nonempty_string())),
    ?CONVERT_ERROR([], (nonempty_string())).

%%=====================================
%% iodata()
%%=====================================
%% @doc iodata() :: iolist() | binary()
%% @end
-type iodata_type() :: iodata().

iodata_validation_test() ->
    ?assertEqual(true, (istype(return_value([1, <<"binary">>, [1 | <<"binary">>] | <<"binary">>]), iodata()))),
    ?assertEqual(true, (istype(return_value([1, <<"binary">>, [1 | <<"binary">>] | <<"binary">>]), iodata_type()))),
    ?assertEqual(true, (istype(return_value([]), iodata()))),
    ?assertEqual(true, (istype(return_value(<<"binary">>), iodata()))),
    ?assertEqual(false, (istype(return_value(1), iodata()))).

iodata_conversion_test() ->
    [1, <<"binary">>] = totype(return_value([1, <<"binary">>]), iodata()),
    [1, <<"binary">>] = totype(return_value([1, <<"binary">>]), iodata_type()),
    [1 | <<"binary">>] = totype(return_value([1 | <<"binary">>]), iodata()),
    [1, <<"binary">>] = totype(return_value([1, binary]), iodata()),
    [1, <<"binary">>] = totype(return_value({1, binary}), iodata()),
    [1, [1, <<"binary">>]] = totype(return_value({1, {1, binary}}), iodata()),
    "1" = totype(return_value(1), iodata()),
    "1.0" = totype(return_value(1.0), iodata()).

%%=====================================
%% iolist()
%%=====================================
%% @doc iolist() :: maybe_improper_list(byte() | binary() | iolist(),
%%                                      binary() | [])
%% @end
-type iolist_type() :: iolist().

iolist_validation_test() ->
    ?assertEqual(true, (istype(return_value([1, <<"binary">>, [1 | <<"binary">>] | <<"binary">>]), iolist()))),
    ?assertEqual(true, (istype(return_value([1, <<"binary">>, [1 | <<"binary">>] | <<"binary">>]), iolist_type()))),
    ?assertEqual(true, (istype(return_value([]), iolist()))),
    ?assertEqual(false, (istype(return_value(1), iolist()))).

iolist_conversion_test() ->
    [1, <<"binary">>] = totype(return_value([1, <<"binary">>]), iolist()),
    [1, <<"binary">>] = totype(return_value([1, <<"binary">>]), iolist_type()),
    [1 | <<"binary">>] = totype(return_value([1 | <<"binary">>]), iolist()),
    [1, <<"binary">>] = totype(return_value([1, binary]), iolist()),
    [1, <<"binary">>] = totype(return_value({1, binary}), iolist()),
    [1, [1, <<"binary">>]] = totype(return_value({1, {1, binary}}), iolist()).

%%=====================================
%% function()
%%=====================================
%% @doc function() :: fun()
%% @end
-type function_type() :: function().

function_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), function_type()))),
    ?assertEqual(true, (istype(return_value(fun return_value/1), function_type()))),
    ?assertEqual(true, (istype(return_value(fun () -> ok end), function_type()))).

function_conversion_test() -> Fun = fun () -> ok end, ?CONVERT_ERROR(atom, (function_type())), ?CONVERT_ERROR(Fun, (function_type())).

%%=====================================
%% module()
%%=====================================
%% @doc module() :: atom()
%% @end
-type module_type() :: module().

module_validation_test() ->
    ?assertEqual(true, (istype(return_value(atom), module()))), ?assertEqual(false, (istype(return_value(<<"binary">>), module()))).

module_conversion_test() ->
    ?assertEqual(atom, (totype(atom, module()))),
    ?assertEqual(atom, (totype(<<"atom">>, module()))),
    ?assertEqual(atom, (totype("atom", module()))),
    ?CONVERT_ERROR(1, (module())).

%%=====================================
%% mfa()
%%=====================================
%% @doc mfa() :: {module(), atom(), arity()}
%% @end
-type mfa_type() :: mfa().

mfa_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), mfa()))),
    ?assertEqual(true, (istype(return_value({atom, atom, 0}), mfa()))),
    ?assertEqual(false, (istype(return_value({<<"binary">>, atom, 0}), mfa()))),
    ?assertEqual(false, (istype(return_value({atom, <<"binary">>, 0}), mfa()))),
    ?assertEqual(false, (istype(return_value({atom, atom, atom}), mfa()))).

mfa_conversion_test() ->
    ?assertEqual({atom, atom, 0}, (totype({atom, atom, 0}, mfa()))),
    ?assertEqual({atom, atom, 0}, (totype({<<"atom">>, <<"atom">>, <<"0">>}, mfa()))),
    ?assertEqual({atom, atom, 0}, (totype({atom, atom, 0.0}, mfa()))),
    ?assertEqual({atom, atom, 0}, (totype({"atom", "atom", "0"}, mfa()))),
    ?assertEqual({atom, atom, 0}, (totype(["atom", "atom", "0"], mfa()))),
    ?CONVERT_ERROR(atom, (mfa())).

%%=====================================
%% arity()
%%=====================================
%% @doc arity() :: 0..255
%% @end
-type arity_type() :: arity().

arity_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), arity()))),
    ?assertEqual(true, (istype(return_value(0), arity()))),
    ?assertEqual(false, (istype(return_value(-1), arity()))),
    ?assertEqual(false, (istype(return_value(256), arity()))).

arity_conversion_test() -> 0 = totype(0, arity()), ?CONVERT_ERROR(atom, (arity())), ?CONVERT_ERROR((-1), (arity())).

%%=====================================
%% identifier()
%%=====================================
%% @doc identifier() :: pid() | port() | refrence()
%% @end
-type identifier_type() :: identifier().

identifier_validation_test() ->
    Self = self(),
    Port = open_port({spawn, "tar -xzf -"}, [exit_status, binary]),
    Ref = make_ref(),
    ?assertEqual(false, (istype(return_value(atom), identifier()))),
    ?assertEqual(true, (istype(return_value(Self), identifier()))),
    ?assertEqual(false, (istype(return_value(atom), identifier()))),
    ?assertEqual(true, (istype(return_value(Port), identifier()))),
    ?assertEqual(false, (istype(return_value(atom), identifier()))),
    ?assertEqual(true, (istype(return_value(Ref), identifier()))),
    ?assertEqual(true, (port_close(Port))).

identifier_conversion_test() ->
    Self = self(),
    ListSelf = pid_to_list(Self),
    BinarySelf = list_to_binary(ListSelf),
    Self = totype(BinarySelf, identifier()),
    Self = totype(ListSelf, identifier()),
    Self = totype(Self, identifier()),
    Port = open_port({spawn, "tar -xzf -"}, [exit_status, binary]),
    ListPort = port_to_list(Port),
    BinaryPort = list_to_binary(ListPort),
    Port = totype(BinaryPort, identifier()),
    Port = totype(ListPort, identifier()),
    Port = totype(Port, identifier()),
    Ref = make_ref(),
    ListRef = ref_to_list(Ref),
    BinaryRef = list_to_binary(ListRef),
    Ref = totype(BinaryRef, identifier()),
    Ref = totype(ListRef, identifier()),
    Ref = totype(Ref, identifier()),
    ?CONVERT_ERROR(atom, (identifier())),
    ?assertEqual(true, (port_close(Port))).

%%=====================================
%% node()
%%=====================================
%% @doc node() :: atom()
%% @end
-type node_type() :: node().

node_validation_test() ->
    ?assertEqual(true, (istype(return_value(atom), node()))), ?assertEqual(false, (istype(return_value(<<"binary">>), node()))).

node_conversion_test() ->
    ?assertEqual(atom, (totype(atom, node()))),
    ?assertEqual(atom, (totype(<<"atom">>, node()))),
    ?assertEqual(atom, (totype("atom", node()))),
    ?CONVERT_ERROR(1, (node())).

%%=====================================
%% timeout()
%%=====================================
%% @doc timeout() :: 'infinity' | non_neg_integer()
%% @end
-type timeout_type() :: timeout().

timeout_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), timeout()))),
    ?assertEqual(true, (istype(return_value(infinity), timeout()))),
    ?assertEqual(true, (istype(return_value(0), timeout()))),
    ?assertEqual(true, (istype(return_value(1), timeout()))),
    ?assertEqual(false, (istype(return_value(-1), timeout()))).

timeout_conversion_test() ->
    infinity = totype(infinity, timeout()),
    infinity = totype(<<"infinity">>, timeout()),
    infinity = totype("infinity", timeout()),
    0 = totype(<<"0">>, timeout()),
    0 = totype(0.0, timeout()),
    0 = totype(0, timeout()),
    0 = totype("0", timeout()),
    ?CONVERT_ERROR(atom, (timeout())),
    ?CONVERT_ERROR((-1), (timeout())).

%%=====================================
%% no_return()
%%=====================================
%% @doc no_return() :: none()
%% @end
-type no_return_type() :: no_return().

no_return_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), no_return()))),
    ?assertEqual(false, (istype(return_value(<<"binary">>), no_return()))),
    ?assertEqual(false, (istype(return_value(1.0), no_return()))),
    ?assertEqual(false, (istype(return_value(1), no_return()))),
    ?assertEqual(false, (istype(return_value("list"), no_return()))).

no_return_conversion_test() ->
    ?CONVERT_ERROR(atom, (no_return())),
    ?CONVERT_ERROR(<<"binary">>, (no_return())),
    ?CONVERT_ERROR(1.0, (no_return())),
    ?CONVERT_ERROR(1, (no_return())),
    ?CONVERT_ERROR("list", (no_return())).

%%=====================================
%% non_neg_integer()
%%=====================================
%% @doc non_neg_integer() :: 0..
%% @end
-type non_neg_integer_type() :: non_neg_integer().

non_neg_integer_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), non_neg_integer()))),
    ?assertEqual(false, (istype(return_value(-1), non_neg_integer()))),
    ?assertEqual(true, (istype(return_value(0), non_neg_integer()))).

non_neg_integer_conversion_test() ->
    0 = totype(<<"0.0">>, non_neg_integer()),
    0 = totype(<<"0">>, non_neg_integer()),
    0 = totype(0.0, non_neg_integer()),
    0 = totype(0, non_neg_integer()),
    0 = totype("0.0", non_neg_integer()),
    0 = totype("0", non_neg_integer()),
    ?CONVERT_ERROR(atom, (non_neg_integer())),
    ?CONVERT_ERROR((-1), (non_neg_integer())).

%%=====================================
%% pos_integer()
%%=====================================
%% @doc pos_integer() :: 1..
%% @end
-type pos_integer_type() :: pos_integer().

pos_integer_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), pos_integer()))),
    ?assertEqual(false, (istype(return_value(0), pos_integer()))),
    ?assertEqual(true, (istype(return_value(1), pos_integer()))).

pos_integer_conversion_test() ->
    ?assertEqual(1, (totype(<<"1.0">>, pos_integer()))),
    ?assertEqual(1, (totype(<<"1">>, pos_integer()))),
    ?assertEqual(1, (totype(1.0, pos_integer()))),
    ?assertEqual(1, (totype(1, pos_integer()))),
    ?assertEqual(1, (totype("1.0", pos_integer()))),
    ?assertEqual(1, (totype("1", pos_integer()))),
    ?CONVERT_ERROR(atom, (pos_integer())),
    ?CONVERT_ERROR(0, (pos_integer())).

%%=====================================
%% neg_integer()
%%=====================================
%% @doc neg_integer() :: ..-1
%% @end
-type neg_integer_type() :: neg_integer().

neg_integer_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), neg_integer()))),
    ?assertEqual(false, (istype(return_value(0), neg_integer()))),
    ?assertEqual(true, (istype(return_value(-1), neg_integer()))).

neg_integer_conversion_test() ->
    ?assertEqual((-1), (totype(<<"-1.0">>, neg_integer()))),
    ?assertEqual((-1), (totype(<<"-1">>, neg_integer()))),
    ?assertEqual((-1), (totype(-1.0, neg_integer()))),
    ?assertEqual((-1), (totype(-1, neg_integer()))),
    ?assertEqual((-1), (totype("-1.0", neg_integer()))),
    ?assertEqual((-1), (totype("-1", neg_integer()))),
    ?CONVERT_ERROR(atom, (neg_integer())),
    ?CONVERT_ERROR(0, (neg_integer())).

%%=====================================
%% nonempty_maybe_improper_list()
%%=====================================
%% @doc nonempty_maybe_improper_list() :: nonempty_maybe_improper_list(any(), any())
%% @end
-type nonempty_maybe_improper_list_type() :: nonempty_maybe_improper_list().

nonempty_maybe_improper_list_validation_test() ->
    ?assertEqual(true, (istype(return_value([1]), nonempty_maybe_improper_list()))),
    ?assertEqual(true, (istype(return_value([1]), nonempty_maybe_improper_list_type()))),
    ?assertEqual(true, (istype(return_value([1 | atom]), nonempty_maybe_improper_list()))),
    ?assertEqual(false, (istype(return_value([]), nonempty_maybe_improper_list()))).

nonempty_maybe_improper_list_conversion_test() ->
    [1, atom] = totype(return_value({1, atom}), nonempty_maybe_improper_list()),
    [1, atom] = totype(return_value({1, atom}), nonempty_maybe_improper_list_type()),
    [1, atom] = totype(return_value([1, atom]), nonempty_maybe_improper_list()),
    [1 | atom] = totype(return_value([1 | atom]), nonempty_maybe_improper_list()),
    ?CONVERT_ERROR([], (nonempty_maybe_improper_list())),
    ?CONVERT_ERROR({}, (nonempty_maybe_improper_list())).

%%=====================================
%% nonempty_improper_list(Type1, Type2)
%%=====================================
%% @doc nonempty_improper_list(Type1, Type2)
%%
%%      Handled by List above.
%% @end

%%=====================================
%% nonempty_maybe_improper_list(Type1, Type2)
%%=====================================
%% @doc nonempty_maybe_improper_list(Type1, Type2)
%% @end
-type typed_nonempty_maybe_improper_list_type() :: nonempty_maybe_improper_list(integer(), atom() | []).

typed_nonempty_maybe_improper_list_validation_test() ->
    ?assertEqual(true, (istype(return_value([1]), typed_nonempty_maybe_improper_list_type()))),
    ?assertEqual(true, (istype(return_value([1]), typed_nonempty_maybe_improper_list_type()))),
    ?assertEqual(true, (istype(return_value([1 | atom]), typed_nonempty_maybe_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([1 | 1]), typed_nonempty_maybe_improper_list_type()))),
    ?assertEqual(false, (istype(return_value([]), typed_nonempty_maybe_improper_list_type()))).

typed_nonempty_maybe_improper_list_conversion_test() ->
    ?assertEqual([1, 1], (totype(return_value({1, 1}), typed_nonempty_maybe_improper_list_type()))),
    ?assertEqual([1, 1], (totype(return_value([1, 1]), typed_nonempty_maybe_improper_list_type()))),
    ?assertEqual([1, 1], (totype(return_value([<<"1">>, "1"]), typed_nonempty_maybe_improper_list_type()))),
    ?assertEqual([1 | atom], (totype(return_value([<<"1">> | <<"atom">>]), typed_nonempty_maybe_improper_list_type()))),
    ?CONVERT_ERROR([1 | 1], (typed_nonempty_maybe_improper_list_type())),
    ?CONVERT_ERROR([atom | atom], (typed_nonempty_maybe_improper_list_type())),
    ?CONVERT_ERROR([], (typed_nonempty_maybe_improper_list_type())),
    ?CONVERT_ERROR({}, (typed_nonempty_maybe_improper_list_type())).

%%=====================================
%% Record
%%=====================================
-type record_a_type() :: #record_a{}.

-type typed_record_a_type() :: #record_a{e :: binary()}.

-type record_b_type() :: #record_b{}.

-type typed_record_b_type() :: #record_b{f :: #record_a{e :: atom()}}.

%-record(record_a, {a = atom :: atom(),
%                   b = atom,
%                   c        :: atom(),
%                   d,
%                   e        :: atom() | binary()}).
%-record(record_b, {f :: #record_a{e :: binary()},
%                   g :: atom() | integer()}).
record_validation_test() ->
    ?assertEqual(false, (istype(return_value(atom), #record_a{}))),
    ?assertEqual(false, (istype(return_value(atom), record_a_type()))),
    ?assertEqual(true, (istype(return_value(#record_a{}), #record_a{}))),
    ?assertEqual(true, (istype(return_value(#record_a{}), record_a_type()))),
    ?assertEqual(true, (istype(return_value(#record_a{a = atom}), #record_a{a = atom}))),
    ?assertEqual(false, (istype(return_value(#record_a{a = also}), #record_a{a = atom}))),
    ?assertEqual(true, (istype(return_value(#record_a{a = also}), #record_a{a = atom()}))),
    ?assertEqual(false, (istype(return_value(#record_a{c = <<"binary">>}), record_a_type()))),
    ?assertEqual(false, (istype(return_value(#record_a{}), typed_record_a_type()))),
    ?assertEqual(true, (istype(return_value(#record_a{e = <<"binary">>}), typed_record_a_type()))),
    ?assertEqual(false, (istype(return_value(#record_a{}), #record_b{}))),
    ?assertEqual(false, (istype(return_value(#record_a{}), record_b_type()))),
    ?assertEqual(false, (istype(return_value(#record_b{}), #record_b{}))),
    ?assertEqual(false, (istype(return_value(#record_b{}), record_b_type()))),
    ?assertEqual(true, (istype(return_value(#record_b{f = #record_a{e = <<"binary">>}}), #record_b{}))),
    ?assertEqual(true, (istype(return_value(#record_b{f = #record_a{e = <<"binary">>}}), record_b_type()))),
    ?assertEqual(true, (istype(return_value(#record_b{f = #record_a{e = <<"binary">>}}), record_b_type()))),
    ?assertEqual(false, (istype(return_value(#record_b{f = #record_a{e = atom}}), record_b_type()))),
    ?assertEqual(false, (istype(return_value(#record_b{f = #record_a{e = <<"binary">>}}), typed_record_b_type()))),
    ?assertEqual(true, (istype(return_value(#record_b{f = #record_a{e = atom}}), typed_record_b_type()))).

list_to_record_conversion_test() ->
    #record_a{a = also, b = atom, c = atom, d = 1.0, e = binary} = totype([{<<"a">>, <<"also">>}, {"c", "atom"}, {<<"d">>, 1.0},
									   {e, <<"binary">>}],
									  #record_a{}),
    #record_a{a = also, b = atom, c = atom, d = 1.0, e = binary} = totype([{<<"a">>, <<"also">>}, {"c", "atom"}, {<<"d">>, 1.0},
									   {e, <<"binary">>}, {f, invalid}],
									  #record_a{}),
    #record_b{f = #record_a{a = also, b = atom, c = atom, d = 1.0, e = binary}, g = atom} = totype([{f,
												     [{<<"a">>, <<"also">>}, {"c", "atom"}, {<<"d">>, 1.0},
												      {e, <<"binary">>}]},
												    {<<"g">>, "atom"}],
												   #record_b{}).

map_to_record_conversion_test() -> ok.

tuple_to_record_converison_test() -> ok.

record_to_list_conversion_test() -> ok.

record_to_map_conversion_test() -> ok.

record_to_record_conversion_test() -> ok.

to_src() -> file:write_file("output.erl", io_lib:format("~s\n", [forms:from_abstract(forms:read(istype_test))])).
