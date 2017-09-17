-module(istype).

%% API exports
-export([return_value/1]).

-include_lib("eunit/include/eunit.hrl").

-record(record0, {a = apple  :: atom(),
                  b = <<"">> :: binary(),
                  c          :: undefined | range0(),
                  d}).

-record(record1, {a = #record0{} :: record0(),
                  b = <<"">> :: binary()}).

-type union0()  :: atom | integer() | binary().
-type union1()  :: float() | union0().
-type union2()  :: tuple1() | tuple3().
-type range0()  :: 1..100.
-type tuple0()  :: tuple().
-type tuple1()  :: {}.
-type tuple2()  :: {atom}.
-type tuple3()  :: {atom, atom()}.
-type tuple4()  :: {atom, atom() | binary()}.
-type record0() :: #record0{}.
-type record1() :: #record1{}.
-type record2() :: #record0{c :: range0()}.
-type list0()   :: list().
-type list1()   :: [].
-type list2()   :: [atom()].
-type list3()   :: list(atom() | integer()).

-export_type([union0/0,
              union1/0,
              union2/0,
              range0/0,
              tuple0/0,
              tuple1/0,
              tuple2/0,
              tuple3/0,
              tuple4/0,
              record0/0,
              record1/0,
              record2/0,
              list0/0,
              list1/0,
              list2/0,
              list3/0]).

%%====================================================================
%% Test functions
%%====================================================================
assert_test() ->
    TestBinary = return_value(<<"binary">>),
    true = asserttype(atom, atom()),
    true = try
               asserttype(TestBinary, atom()),
               false
           catch
               error:{badmatch, false} ->
                   true
           end.

invocation_test() ->
    TestAtom = return_value(atom),
    TestRecord = #record0{},
    true = istype(TestRecord#record0.a, atom()),
    true = istype(atom, atom()),
    true = istype(TestAtom, atom()),
    true = istype(fun() -> atom end(), atom()),
    true = istype(return_value(atom), atom()),
    true = istype(?MODULE:return_value(atom), atom()),
    true = istype([X1 || X1 <- lists:seq(0, 1)], list()),
    true = istype(case atom of X0 -> X0 end, atom()),
    true = istype(begin return_value(atom) end, atom()),
    true = istype(try 1 = 1 catch _:_ -> undefined end, integer()).

guard_test() ->
    TestAtom = return_value(atom),
    true = case ?MODULE:return_value(atom) of
               X0 when istype(X0, atom()) -> true
           end,

    Record1 = return_value(#record1{}),
    true = case Record1 of
               X1 when istype(X1, record1()) -> true
           end,

    true = fun(X2) when istype(X2, atom()) -> true;
              (_) -> false
           end(TestAtom).

return_value(Value) -> Value.

atom_test() ->
    true = istype(return_value(atom), atom()),
    false = istype(return_value(<<"binary">>), atom()),

    Atom = return_value(atom),
    Atom = totype(atom, atom()),
    Atom = totype(<<"atom">>, atom()),
    Atom = totype("atom", atom()).

binary_test() ->
    false = istype(return_value(atom), binary()),
    true = istype(return_value(<<"binary">>), binary()),

    <<"binary">> = totype(binary, binary()),
    <<"binary">> = totype(<<"binary">>, binary()),
    <<"1.00000000000000000000e+00">> = totype(1.0, binary()),
    <<"1">> = totype(1, binary()),
    <<"binary">> = totype("binary", binary()),
    <<"#Fun<istype", _/binary>> = totype(fun() -> atom end, binary()),

    Ref = make_ref(),
    BinaryRef = list_to_binary(ref_to_list(Ref)),
    BinaryRef = totype(Ref, binary()),

    Pid = self(),
    BinaryPid = list_to_binary(pid_to_list(Pid)),
    BinaryPid = totype(Pid, binary()).

bitstring_test() ->
    false = istype(atom, bitstring()),
    true = istype(<<"binary">>, bitstring()),

    <<"bitstring">> = totype(<<"bitstring">>, bitstring()),
    <<"bitstring">> = totype("bitstring", bitstring()).

boolean_test() ->
    true = istype(return_value(true), boolean()),
    true = istype(return_value(false), boolean()),
    false = istype(return_value(undefined), boolean()),
    false = istype(return_value(<<"binary">>), boolean()),

    true = totype(true, boolean()),
    true = totype(<<"true">>, boolean()),
    true = totype("true", boolean()).

float_test() ->
    false = istype(return_value(atom), float()),
    true = istype(return_value(1.0), float()),
    false = istype(return_value(1), float()),

    1.0 = totype(<<"1">>, float()),
    1.0 = totype(<<"1.0">>, float()),
    1.0 = totype(<<"1.00000000e+00">>, float()),
    1.0 = totype(1.0, float()),
    1.0 = totype(1, float()),
    1.0 = totype("1", float()),
    1.0 = totype("1.0", float()),
    1.0 = totype("1.00000000e+00", float()).

integer_test() ->
    false = istype(return_value(atom), integer()),
    false = istype(return_value(1.0), integer()),
    true = istype(return_value(1), integer()),

    1 = totype(<<"1">>, integer()),
    1 = totype(<<"1.0">>, integer()),
    1 = totype(<<"1.00000000e+00">>, integer()),
    1 = totype(1, integer()),
    1 = totype("1", integer()),
    1 = totype("1.0", integer()),
    1 = totype("1.00000000e+00", integer()).

list_test() ->
    false = istype(return_value(atom), list0()),
    true = istype(return_value("list"), list0()),

    "atom" = totype(atom, list0()),
    "binary" = totype(<<"binary">>, list0()),
    "1.00000000000000000000e+00" = totype(1.0, list0()),
    "#Fun<istype" ++ _ = totype(fun() -> atom end, list0()),
    "1" = totype(1, list0()),
    "list" = totype("list", list0()),
    [{a, b},
     {b, c}] = totype(#{a => b, b => c}, list0()),

    Pid = self(),
    ListPid = pid_to_list(Pid),
    ListPid = totype(Pid, list0()),

    Ref = make_ref(),
    ListRef = ref_to_list(Ref),
    ListRef = totype(Ref, list0()),

    [a, b, c] = totype({a, b, c}, list0()),

    true = istype(return_value([]), list1()),
    false = istype(return_value([atom]), list1()),
    false = istype(return_value("string"), list1()),

    [] = totype("", list1()),
    ok = try
             totype([atom], list1())
         catch
             error:{istype_conversion, _, _} ->
                 ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(return_value([]), list2()),
    true = istype(return_value([atom]), list2()),
    true = istype(return_value([atom, also]), list2()),
    false = istype(return_value([1]), list2()),
    false = istype(return_value([atom, 1]), list2()),

    [] = totype({}, list2()),
    [atom] = totype({<<"atom">>}, list2()),
    [atom] = totype(["atom"], list2()),
    ok = try
             totype([1], list2())
         catch
             error:{istype_conversion, _, _} ->
                 ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(return_value([]), list3()),
    true = istype(return_value([atom]), list3()),
    true = istype(return_value([atom, also]), list3()),
    true = istype(return_value([1]), list3()),
    true = istype(return_value([atom, 1]), list3()),
    false = istype(return_value(<<"binary">>), list3()),
    false = istype(return_value([<<"binary">>]), list3()),

    [] = totype({}, list3()),
    [atom] = totype({<<"atom">>}, list3()),
    [atom] = totype(["atom"], list3()),
    [1] = totype(["1"], list3()),
    [1, atom] = totype(["1", <<"atom">>], list3()),

    ok = try
             totype([{}], list3())
         catch
             error:{istype_conversion, _, _} ->
                 ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(return_value([atom]), nonempty_list()),
    false = istype(return_value([]), nonempty_list()),
    false = istype(return_value(atom), nonempty_list()),

    [atom] = totype({atom}, nonempty_list()),
    ok = try
             totype(<<"">>, nonempty_string())
         catch
             error:{istype_conversion, _, _} ->
                 ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(return_value(""), string()),
    true = istype(return_value("string"), string()),
    false = istype(return_value(atom), string()),
    false = istype(return_value([atom]), string()),

    "string" = totype(<<"string">>, string()),
    "string" = totype(string, string()),

    true = istype(return_value("string"), nonempty_string()),
    false = istype(return_value(""), nonempty_string()),

    "string" = totype(<<"string">>, nonempty_string()),
    ok = try
             totype(<<"">>, nonempty_string())
         catch
             error:{istype_conversion, _, _} ->
                 ok;
             _:_ ->
                 wrong_error
         end,

    false = istype(return_value([atom|atom]), list(atom())),
    [atom, binary] = totype(["atom" | <<"binary">>], list(atom())),
    ok = try
             totype(["atom" | 1], list(atom()))
         catch
             error:{istype_conversion, _, _} ->
                 ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(return_value(["string" | <<"binary">>]), iolist()).

map_test() ->
    false = istype(return_value(atom), map()),
    true = istype(return_value(#{}), map()),

    #{a := b,
      b := c} = totype([{a, b}, {b, c}], map()),
    #{a := b,
      b := c} = totype(#{a => b, b => c}, map()).

    %% TODO: record to map

number_test() ->
    false = istype(return_value(atom), number()),
    true = istype(return_value(1.0), number()),
    true = istype(return_value(1), number()),

    1.0 = totype(<<"1.0">>, number()),
    1.0 = totype(1.0, number()),
    1 = totype(1, number()),
    1 = totype("1", number()).

pid_test() ->
    Pid = self(),
    ListPid = pid_to_list(Pid),
    BinaryPid = list_to_binary(ListPid),

    false = istype(return_value(atom), pid()),
    true = istype(return_value(Pid), pid()),
    true = istype(return_value(Pid), identifier()),

    Pid = totype(BinaryPid, pid()),
    Pid = totype(ListPid, pid()),
    Pid = totype(Pid, pid()).

range_test() ->
    false = istype(atom, range0()),
    false = istype(0, range0()),
    true = istype(1, range0()),
    true = istype(50, range0()),
    false = istype(50.0, range0()),
    true = istype(100, range0()),
    false = istype(101, range0()),

    1 = totype(<<"1">>, range0()),
    1 = totype(1.0, range0()),
    1 = totype(1, range0()),
    1 = totype("1", range0()),

    ok = try
             totype(0, range0())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(0, arity()),
    false = istype(-1, arity()),
    false = istype(256, arity()),
    0 = totype(0, arity()),
    ok = try
             totype(-1, arity())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(0, byte()),
    false = istype(-1, byte()),
    false = istype(256, byte()),
    0 = totype(0, byte()),
    ok = try
             totype(-1, byte())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(0, char()),
    false = istype(-1, char()),
    false = istype(16#110000, char()),
    0 = totype(0, char()),
    ok = try
             totype(-1, char())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(0, non_neg_integer()),
    false = istype(-1, non_neg_integer()),
    0 = totype(0, non_neg_integer()),
    ok = try
             totype(-1, non_neg_integer())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(1, pos_integer()),
    false = istype(0, pos_integer()),
    1 = totype(1, pos_integer()),
    ok = try
             totype(0, pos_integer())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    true = istype(-1, neg_integer()),
    false = istype(0, neg_integer()),
    -1 = totype(-1, neg_integer()),
    ok = try
             totype(0, neg_integer())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end.

record_test() ->
    %-record(record0, {a = apple  :: atom(),
    %                  b = <<"">> :: binary(),
    %                  c          :: undefined | range0(),
    %                  d}).
    %
    %-record(record1, {a = #record0{} :: record0(),
    %              b = <<"">> :: binary()}).
    false = istype(return_value(atom), record0()),
    true = istype(return_value(#record0{}), record0()),
    true = istype(return_value(#record0{}), #record0{}),
    true = istype(return_value(#record0{a = atom,
                                        b = <<"binary">>,
                                        c = 50,
                                        d = also}),
                  record0()),
    true = istype(return_value({record0, atom, <<"">>, undefined, undefined}), record0()),
    false = istype(return_value({record0, atom, <<"">>, atom, undefined}), record0()),

    #record0{a = atom,
             b = <<"binary">>,
             c = 50,
             d = atom} = totype({record0, "atom", "binary", "50", atom}, record0()),
    #record0{a = atom,
             b = <<"binary">>,
             c = 50,
             d = atom} = totype(#{a => "atom", b => "binary", c => "50", d => atom}, record0()),
    #record0{a = atom,
             b = <<"binary">>,
             c = 50,
             d = atom} = totype(#{"a" => "atom", <<"b">> => "binary", "c" => "50", <<"d">> => atom}, record0()),
    #record0{a = atom,
             b = <<"binary">>,
             c = 50,
             d = atom} = totype([{a, "atom"}, {b, "binary"}, {c, "50"}, {d, atom}], record0()),

    false = istype(return_value(atom), record1()),
    true = istype(return_value(#record1{}), record1()),
    true = istype(return_value(#record1{a = #record0{a = atom,
                                                     b = <<"binary">>,
                                                     c = 50,
                                                     d = also},
                                        b = <<"binary">>}),
                  record1()),
    true = istype(return_value({record1, {record0, atom, <<"">>, undefined, undefined}, <<"">>}), record1()),
    false = istype(return_value({record1, {record0, atom, <<"">>, undefined, undefined}, undefined}), record1()),
    false = istype(return_value({record1, {record0, atom, <<"">>, atom, undefined}, <<"">>}), record1()),

    #record1{a = #record0{a = atom,
                          b = <<"binary">>,
                          c = 50,
                          d = atom},
             b = <<"binary">>} = totype({record1, {record0, "atom", "binary", "50", atom}, "binary"}, record1()),
    #record1{a = #record0{a = atom,
                          b = <<"binary">>,
                          c = 50,
                          d = atom},
             b = <<"binary">>} = totype(#{a => #{a => "atom",
                                                 b => "binary",
                                                 c => "50",
                                                 d => atom},
                                          b => binary},
                                        record1()),
    #record1{a = #record0{a = atom,
                          b = <<"binary">>,
                          c = 50,
                          d = atom},
             b = <<"binary">>} = totype(#{a => #{"a" => "atom",
                                                 <<"b">> => "binary",
                                                 "c" => "50",
                                                 <<"d">> => atom},
                                          b => binary},
                                        record1()),
    #record1{a = #record0{a = atom,
                          b = <<"binary">>,
                          c = 50,
                          d = atom},
             b = <<"binary">>} = totype([{a, [{a, "atom"},
                                              {b, "binary"},
                                              {c, "50"},
                                              {d, atom}]},
                                         {b, binary}],
                                        record1()),

    false = istype(return_value(#record0{}), record2()),
    true = istype(return_value(#record0{c = 1}), record2()),

    #record0{c = 1} = totype(#record0{c = <<"1">>}, record2()),
    ok = try
             totype(#record0{}, record2())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end.

reference_test() ->
    Ref = make_ref(),
    ListRef = ref_to_list(Ref),
    BinaryRef = list_to_binary(ListRef),

    false = istype(return_value(atom), reference()),
    true = istype(return_value(Ref), reference()),
    true = istype(return_value(Ref), identifier()),

    Ref = totype(BinaryRef, reference()),
    Ref = totype(ListRef, reference()),
    Ref = totype(Ref, reference()).

tuple_test() ->
    %-type tuple0()  :: tuple().
    false = istype(return_value(atom), tuple0()),
    true = istype(return_value({}), tuple0()),
    true = istype(return_value({atom}), tuple0()),

    {} = totype({}, tuple0()),
    {} = totype([], tuple0()),
    {atom} = totype({atom}, tuple0()),

    %-type tuple1()  :: {}.
    false = istype(return_value(atom), tuple1()),
    true = istype(return_value({}), tuple1()),
    false = istype(return_value({atom}), tuple1()),

    {} = totype({}, tuple1()),
    {} = totype([], tuple1()),
    ok = try
             totype([atom], tuple1())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    %-type tuple2()  :: {atom}.
    false = istype(return_value(atom), tuple2()),
    false = istype(return_value({}), tuple2()),
    true = istype(return_value({atom}), tuple2()),
    false = istype(return_value({also}), tuple2()),
    false = istype(return_value({atom, atom}), tuple2()),

    {atom} = totype({atom}, tuple2()),
    {atom} = totype([<<"atom">>], tuple2()),
    ok = try
             totype([also], tuple2())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,
    ok = try
             totype([atom, <<"also">>], tuple2())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    %-type tuple3()  :: {atom, atom()}.
    false = istype(return_value(atom), tuple3()),
    false = istype(return_value({atom}), tuple3()),
    true = istype(return_value({atom, atom}), tuple3()),
    true = istype(return_value({atom, also}), tuple3()),
    false = istype(return_value({also, also}), tuple3()),
    false = istype(return_value({atom, atom, atom}), tuple3()),

    {atom, also} = totype({atom, also}, tuple3()),
    {atom, also} = totype([<<"atom">>, "also"], tuple3()),
    ok = try
             totype([also, <<"atom">>], tuple3())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,
    ok = try
             totype(["atom", 12], tuple3())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,

    %-type tuple4()  :: {atom, atom() | binary()}.
    false = istype(return_value(atom), tuple4()),
    false = istype(return_value({atom}), tuple4()),
    true = istype(return_value({atom, atom}), tuple4()),
    true = istype(return_value({atom, also}), tuple4()),
    true = istype(return_value({atom, <<"binary">>}), tuple4()),
    false = istype(return_value({also, atom}), tuple4()),
    false = istype(return_value({atom, 1}), tuple4()),
    false = istype(return_value({atom, atom, atom}), tuple4()),


    {atom, atom} = totype({atom, atom}, tuple4()),
    {atom, also} = totype({atom, <<"also">>}, tuple4()),
    {atom, also} = totype([<<"atom">>, "also"], tuple4()),
    {atom, <<"1">>} = totype([<<"atom">>, 1], tuple4()),
    ok = try
             totype([also, <<"atom">>], tuple4())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end,
    ok = try
             totype(["atom", {}], tuple4())
         catch
             error:{istype_conversion, _, _} ->
                ok;
             _:_ ->
                 wrong_error
         end.

union_test() ->
    true = istype(atom, union0()),
    false = istype(also, union0()),
    true = istype(<<"binary">>, union0()),
    false = istype(1.0, union0()),
    true = istype(1, union0()),

    atom = totype(atom, union0()),
    atom = totype(<<"atom">>, union0()),
    1 = totype(1, union0()),
    1 = totype("1.0", union0()),
    <<"also">> = totype(also, union0()),
    <<"binary">> = totype("binary", union0()),

    true = istype(atom, union1()),
    false = istype(also, union1()),
    true = istype(<<"binary">>, union1()),
    true = istype(1.0, union1()),
    true = istype(1, union1()),

    atom = totype(atom, union1()),
    atom = totype(<<"atom">>, union1()),
    1.0 = totype(1, union1()),
    1.0 = totype("1.0", union1()),
    <<"also">> = totype(also, union1()),
    <<"binary">> = totype("binary", union1()),

    true = istype(infinity, timeout()),
    true = istype(0, timeout()),
    true = istype(1, timeout()),
    false = istype(-1, timeout()).

