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


-record(record2, {a = #record1{} :: record1()}).

-type union0()  :: atom | integer() | binary().
-type union1()  :: float() | union0().
-type union2()  :: tuple1() | tuple3().
-type union3()  :: integer() | tuple().
-type range0()  :: 1..100.
-type tuple0()  :: {}.
-type tuple1()  :: {atom}.
-type tuple2()  :: {atom, atom()}.
-type tuple3()  :: {atom, atom(), binary()}.
-type tuple4()  :: {atom() | integer()}.
-type record0() :: #record0{}.
-type record1() :: #record1{}.
-type record2() :: #record2{}.

-export_type([union0/0,
              union1/0,
              union2/0,
              union3/0,
              range0/0,
              tuple0/0,
              tuple1/0,
              tuple2/0,
              tuple3/0,
              tuple4/0,
              record0/0,
              record1/0,
              record2/0]).

-define(validate(Expected, Type), lists:foreach(fun(Value) ->
                                                    io:format("~p = istype(~p, ~p())\n", [Expected, Value, Type]),
                                                    Expected = istype(Value, Type())
                                                end,
                                                Type(Expected))).

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
    
    true = case hd(record2(true)) of
               X1 when istype(X1, record2()) -> true
           end,

    true = fun(X2) when istype(X2, atom()) -> true;
              (_) -> false
           end(TestAtom).

union_test() ->
    ?validate(true, union0),
    ?validate(false, union0),

    ?validate(true, union1),
    ?validate(false, union1),

    ?validate(true, union2),
    ?validate(false, union2),

    ?validate(true, union3),
    ?validate(false, union3).

range_test() ->
    ?validate(true, range0),
    ?validate(false, range0).

record_test() ->
    ?validate(true, record0),
    ?validate(false, record0),

    ?validate(true, record1),
    ?validate(false, record1),

    ?validate(true, record2),
    ?validate(false, record2).

tuple_test() ->
    Val = return_value({a, b, c}),
    true = istype(Val, tuple()),

    ?validate(true,  tuple0),
    ?validate(false, tuple0),

    ?validate(true,  tuple1),
    ?validate(false, tuple1),

    ?validate(true,  tuple2),
    ?validate(false, tuple2),

    ?validate(true,  tuple3),
    ?validate(false, tuple3),

    ?validate(true,  tuple4),
    ?validate(false, tuple4).

totype_test() ->
    atom = totype(atom, atom()),    
    atom = totype(<<"atom">>, atom()),
    atom = totype("atom", atom()),

    <<"binary">> = totype(binary, binary()),
    <<"binary">> = totype(<<"binary">>, binary()),
    <<"1.00000000000000000000e+00">> = totype(1.0, binary()),
    <<"1">> = totype(1, binary()),
    <<"binary">> = totype("binary", binary()),
    BinaryPid = list_to_binary(pid_to_list(self())),
    BinaryPid = totype(self(), binary()),
    
    true = totype(true, boolean()),
    false = totype(false, boolean()),
    true = totype(<<"true">>, boolean()),
    true = totype("true", boolean()),

    1.0 = totype(<<"1.0">>, float()),
    1.0 = totype(1.0, float()),
    1.0 = totype(1, float()),
    1.0 = totype("1.0", float()),

    1 = totype(<<"1">>, integer()),
    1 = totype(1.0, integer()),
    1 = totype(1, integer()),
    1 = totype("1", integer()),

    "list" = totype(list, list()),
    "list" = totype(<<"list">>, list()),
    "1.00000000000000000000e+00" = totype(1.0, list()),
    "1" = totype(1, list()),
    [{a, b}] = totype(#{a => b}, list()),
    ListPid = pid_to_list(self()),
    ListPid = totype(self(), list()),
    [1] = totype({1}, list()),

    #{a := b, b := c} = totype([{a, b}, {b, c}], map()),
    #{a := b, b := c} = totype(#{a => b, b => c}, map()),

    Pid = self(),
    Pid = totype(list_to_binary(pid_to_list(self())), pid()),
    Pid = totype(pid_to_list(self()), pid()),
    Pid = totype(self(), pid()),

    Ref = make_ref(),
    Ref = totype(list_to_binary(ref_to_list(Ref)), reference()),
    Ref = totype(ref_to_list(Ref), reference()),
    Ref = totype(Ref, reference()),

    50 = totype(<<"50">>, range0()),
    true = try
               0 = totype(<<"0">>, range0()),
               false
           catch
               error:{totype_conversion, <<"0">>, {range, 1, 100}} ->
                   true;
               _:_ ->
                   false
           end,

    atom = totype(<<"atom">>, union0()),
    1 = totype("1", union0()),
    <<"binary">> = totype(binary, union0()),
    1 = totype(<<"1.0">>, union0()),
    
    atom = totype(<<"atom">>, union1()),
    1.0 = totype(<<"1">>, union1()),
    <<"binary">> = totype(binary, union1()),
    1.0 = totype(<<"1.0">>, union1()),

    {a, b, c} = totype([a,b,c], tuple()),
    {}     = totype([], tuple0()),
    {atom} = totype([atom], tuple1()),
    {atom} = totype([<<"atom">>], tuple1()),
    {atom} = totype({<<"atom">>}, tuple1()),

    #record0{} = totype([], record0()),
    #record0{} = totype(#{}, record0()),
    #record0{a = also} = totype([{"a", <<"also">>}], record0()),
    #record0{a = also} = totype(#{<<"a">> => "also"}, record0()),

    #record1{a = #record0{a = also, b = <<"binary">>, c = undefined}} = totype([{a, #{a => "also", b => binary}}], record1()),
    #record1{a = #record0{a = also, b = <<"binary">>, c = undefined}} = totype(#{a => #{a => "also", b => binary}}, record1()),
    #record1{a = #record0{a = also}} = totype([{a, #record0{a = also}}], record1()),

    #record2{a = #record1{b = <<"binary">>}} = totype(#record2{a = #record1{b = binary}}, record2()),

    [{a, [{a, [{a, apple},
               {b, <<"">>},
               {c, undefined},
               {d, {dog}}]},
          {b, <<"">>}]}] = totype(#record2{a=#record1{a=#record0{d = {dog}}}}, list()),

    #{a := #{a := #{a := apple,
                    b := <<"">>,
                    c := undefined,
                    d := {dog}},
             b := <<"">>}} = totype(#record2{a=#record1{a=#record0{d = {dog}}}}, map()).

%%====================================================================
%% Utility functions
%%====================================================================
return_value(Val) ->
    Val.

union0(true) ->
    [<<"binary">>, <<"also">>, 1, 2, atom];
union0(false) ->
    [other, "list", 1.0, {}].

union1(true) ->
    [1.0 | union0(true)];
union1(false) ->
    [other, "list", {}].

union2(true) ->
    tuple1(true) ++ tuple3(true);
union2(false) ->
    tuple1(false) ++ tuple3(false).

union3(true) ->
    [{}, {0}, {0, 1}, 1, 2, 3];
union3(false) ->
    ["", <<"">>, 1.0].

range0(true) ->
    [1, 100, 55];
range0(false) ->
    [0, 101].

record0(true) ->
    [#record0{},
     #record0{a = also,
              b = <<"also">>,
              c = 50}];
record0(false) ->
    [bad_value, {record0}, {record1, a, b, c, d}, #record0{c = atom}].

record1(true) ->
    [#record1{},
     #record1{b = <<"also">>}];
record1(false) ->
    [bad_value, {record1}, {record2, a, b}, #record1{a = undefined}, #record1{a = #record0{a = 0}}].

record2(true) ->
    [#record2{}];
record2(false) ->
    [bad_value, {record2}, {record3, a}, #record2{a = #record1{a = #record0{c = 101}}}].

tuple0(true) -> 
    [{}];
tuple0(false) ->
    [bad_value, {1}].

tuple1(true) ->
    [{atom}];
tuple1(false) ->
    [bad_value, {1}, {1, 2}].

tuple2(true) ->
    [{atom, atom}, {atom, also}];
tuple2(false) ->
    [bad_value, {also, also}, {atom, <<"binary">>}, {atom, atom, <<"binary">>}].

tuple3(true) ->
    [{atom, atom, <<"binary">>}, {atom, also, <<"also">>}];
tuple3(false) ->
    [bad_value, {also, atom, <<"binary">>}, {atom, atom, atom}, {atom, atom, <<"binary">>, <<"binary">>}].

tuple4(true) ->
    [{atom}, {also}, {1}, {2}];
tuple4(false) ->
    [bad_value, {"list"}, {1.0}, {atom, 1}].
