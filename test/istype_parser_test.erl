-module(istype_parser_test).

-include("istype.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(assert_type_equal(Code, Expected),
        ?_test(begin
                   Term = forms:to_abstract(Code),
                   Type = istype_parser:parse_type(?MODULE, Term),
                   ?assertEqual(Expected, Type)
               end)).
parse_type_def_test_() ->
    [?assert_type_equal(
        "-type typename() :: any().",
        #type{module = erlang,
              type   = any,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: none().",               
        #type{module = erlang,
              type   = none,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: pid().",                
        #type{module = erlang,
              type   = pid,
              spec   = [], 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: port().",               
        #type{module = erlang, 
              type   = port, 
              spec   = [], 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: reference().",          
        #type{module = erlang, 
              type   = reference, 
              spec   = [], 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: [].",                   
        #literal{value = {nil, 1}}),
     ?assert_type_equal(
        "-type typename() :: atom().",               
        #type{module = erlang,
              type   = atom, 
              spec   = [], 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: atom.",                 
        #literal{value = {atom, 1, atom}}),
     ?assert_type_equal(
        "-type typename() :: <<_:4>>.",              
        #type{module = erlang,
              type   = bitstring, 
              spec   = {4, 0}, 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: <<_:_*8>>.",            
        #type{module = erlang, 
              type   = bitstring, 
              spec   = {0, 8}, 
            params = []}),
     ?assert_type_equal(
        "-type typename() :: <<_:4, _:_*8>>.",       
        #type{module = erlang, 
              type   = bitstring, 
              spec   = {4, 8}, 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: fun().",                
        #type{module = erlang, 
              type   = 'fun', 
              spec   = any, 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: fun(() -> atom()).",    
        #type{module = erlang, 
              type   = 'fun',      
              spec   = {[],
                        #type{module = erlang, type = atom, spec = [], params = []}}, 
              params = []}),
     ?assert_type_equal(
        "-type typename() :: fun((...) -> atom()).", 
        #type{module = erlang, 
              type   = 'fun',      
              spec   = {any,
                        #type{module = erlang, type = atom, spec = [], params = []}},  
              params = []}),
     ?assert_type_equal(
        "-type typename() :: fun((atom()) -> atom()).",                
        #type{module = erlang, 
              type   = 'fun',      
              spec   = {[#type{module = erlang, type = atom, spec = [], params = []}],
                        #type{module = erlang, type = atom, spec = [], params = []}},  
              params = []}),
     ?assert_type_equal(
        "-type typename() :: fun((atom(), atom()) -> atom()).",                
        #type{module = erlang,
              type   = 'fun',      
              spec   = {[#type{module = erlang, type = atom, spec = [], params = []},
                         #type{module = erlang, type = atom, spec = [], params = []}],
                        #type{module = erlang, type = atom, spec = [], params = []}},  
              params = []}),
     ?assert_type_equal(
        "-type typename() :: integer().",
        #type{module = erlang,
              type   = integer,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: -1.",
        #literal{value = {op, 1, '-', {integer, 1, 1}}}),
     ?assert_type_equal(
        "-type typename() :: 1.",
        #literal{value = {integer, 1, 1}}),
     ?assert_type_equal(
        "-type typename() :: -1..1.",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = -1},
                        #literal{value = 1}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: list(atom()).",
        #type{module = erlang,
              type   = list,
              spec   = {maybe_empty,
                        #type{module = erlang, type = atom, spec = [], params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: maybe_improper_list(atom(), binary()).",
        #type{module = erlang,
              type   = list,
              spec   = {maybe_empty,
                        #type{module = erlang, type = atom, spec = [], params = []},
                        #type{module = erlang, type = binary, spec = [], params = []}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: nonempty_improper_list(atom(), binary()).",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = atom, spec = [], params = []},
                        #type{module = erlang, type = binary, spec = [], params = []}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: nonempty_list(atom()).",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = atom, spec = [], params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: map().",
        #type{module = erlang,
              type   = map,
              spec   = any,
              params = []}),
     ?assert_type_equal(
        "-type typename() :: #{}.",
        #type{module = erlang,
              type   = map,
              spec   = empty,
              params = []}),
     ?assert_type_equal(
        "-type typename() :: #{atom := binary(), atom() := binary()}.",
        #type{module = erlang,
              type   = map,
              spec   = {[{#literal{value = {atom, 1, atom}},
                          #type{module = erlang, type = binary, spec = [], params = []}},
                         {#type{module = erlang, type = atom, spec = [], params = []},
                          #type{module = erlang, type = binary, spec = [], params = []}}],
                        []},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: #{binary => atom(), binary() => atom()}.",
        #type{module = erlang,
              type   = map,
              spec   = {[],
                        [{#literal{value = {atom, 1, binary}},
                          #type{module = erlang, type = atom, spec = [], params = []}},
                         {#type{module = erlang, type = binary, spec = [], params = []},
                          #type{module = erlang, type = atom, spec = [], params = []}}]},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: #{atom := binary(), binary => atom()}.",
        #type{module = erlang,
              type   = map,
              spec   = {[{#literal{value = {atom, 1, atom}},
                          #type{module = erlang, type = binary, spec = [], params = []}}],
                        [{#literal{value = {atom, 1, binary}},
                          #type{module = erlang, type = atom, spec = [], params = []}}]},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: tuple().",
        #type{module = erlang,
              type   = tuple,
              spec   = any,
              params = []}),
     ?assert_type_equal(
        "-type typename() :: {}.",
        #type{module = erlang,
              type   = tuple,
              spec   = empty,
              params = []}),
     ?assert_type_equal(
        "-type typename() :: {atom}.",
        #type{module = erlang,
              type   = tuple,
              spec   = {1,
                        [{1, #literal{value = {atom, 1, atom}}}]},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: {atom, atom()}.",
        #type{module = erlang,
              type   = tuple,
              spec   = {2,
                        [{1, #literal{value = {atom, 1, atom}}},
                         {2, #type{module = erlang, type = atom, spec = [], params = []}}]},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: atom() | binary().",
        #type{module = erlang,
              type   = union,
              spec   = [#type{module = erlang, type = atom, spec = [], params = []},
                        #type{module = erlang, type = binary, spec = [], params = []}],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: atom | binary().",
        #type{module = erlang,
              type   = union,
              spec   = [#literal{value = {atom, 1, atom}},
                        #type{module = erlang, type = binary, spec = [], params = []}],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: binary().",
        #type{module = erlang,
              type   = binary,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: bitstring().",
        #type{module = erlang,
              type   = bitstring,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: boolean().",
        #type{module = erlang,
              type   = boolean,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: byte().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = 255}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: char().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = 16#10ffff}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: nil().",
        #literal{value = {nil, 1}}),
     ?assert_type_equal(
        "-type typename() :: number().",
        #type{module = erlang,
              type   = number,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: list().",
        #type{module = erlang,
              type   = list,
              spec   = any,
              params = []}),
     ?assert_type_equal(
        "-type typename() :: maybe_improper_list().",
        #type{module = erlang,
              type   = list,
              spec   = {maybe_empty,
                        #type{module = erlang, type = any, spec = [], params = []},
                        #type{module = erlang, type = any, spec = [], params = []}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: nonempty_list().",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = any, spec = [], params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: string().",
        #type{module = erlang,
              type   = list,
              spec   = {maybe_empty,
                        #type{module = erlang,
                              type   = range,
                              spec   = {#literal{value = 0},
                                        #literal{value = 16#10ffff}},
                              params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: nonempty_string().",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang,
                              type   = range,
                              spec   = {#literal{value = 0},
                                        #literal{value = 16#10ffff}},
                              params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: iodata().",
        #type{module = erlang,
              type   = union,
              spec   = [#type{module = erlang, type = iolist, spec = [], params = []},
                        #type{module = erlang, type = binary, spec = [], params = []}],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: iolist().",
        #type{module = erlang,
              type   = iolist,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: function().",
        #type{module = erlang,
              type   = 'fun',
              spec   = any,
              params = []}),
     ?assert_type_equal(
        "-type typename() :: module().",
        #type{module = erlang,
              type   = atom,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: mfa().",
        #type{module = erlang,
              type   = tuple,
              spec   = {3,
                        [{1, #type{module = erlang, type = atom, spec = [], params = []}},
                         {2, #type{module = erlang, type = atom, spec = [], params = []}},
                         {3, #type{module = erlang,
                                   type   = range,
                                   spec   = {#literal{value = 0},
                                             #literal{value = 255}},
                                   params = []}}]},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: arity().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = 255}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: identifier().",
        #type{module = erlang,
              type   = union,
              spec   = [#type{module = erlang, type = pid, spec = [], params = []},
                        #type{module = erlang, type = port, spec = [], params = []},
                        #type{module = erlang, type = reference, spec = [], params = []}],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: node().",
        #type{module = erlang,
              type   = atom,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: timeout().",
        #type{module = erlang,
              type   = union,
              spec   = [#literal{value = {atom, 1, infinity}},
                        #type{module = erlang,
                              type   = range,
                              spec   = {#literal{value = 0},
                                        #literal{value = undefined}},
                              params = []}],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: no_return().",
        #type{module = erlang,
              type   = none,
              spec   = [],
              params = []}),
     ?assert_type_equal(
        "-type typename() :: non_neg_integer().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = undefined}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: pos_integer().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 1},
                        #literal{value = undefined}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: neg_integer().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = undefined},
                        #literal{value = -1}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: nonempty_maybe_improper_list().",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = any, spec = [], params = []},
                        #type{module = erlang, type = any, spec = [], params = []}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: nonempty_maybe_improper_list(atom(), atom()).",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = atom, spec = [], params = []},
                        #type{module = erlang, type = atom, spec = [], params = []}},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: #record{}.",
        #type{module = erlang,
              type   = record,
              spec   = {record, []},
              params = []}),
     ?assert_type_equal(
        "-type typename() :: #record{a :: binary()}.",
        #type{module = erlang,
              type   = record,
              spec   = {record, [{a, #type{module = erlang, type = binary, spec = [], params = []}}]},
              params = []})].

-define(assert_types_equal(Code, Expected, Init),
        ?_test(begin
                   file:write_file("./assert_types_equal.test", Code),
                   Terms = forms:read("./assert_types_equal.test"),
                   Types = istype_parser:parse_types(Terms, Init),
                   ?assertEqual(Expected, Types)
               end)).
parse_types_def_test_() ->
    Tests = [
        ?assert_types_equal(
            "-module(test).\n"
            "-type atom_alias() :: atom().\n",
            #{{test, atom_alias, 0} => #type{module = erlang, type = atom, spec = [], params = []}},
            #{}),
        ?assert_types_equal(
            "-module(test).\n"
            "-type atom_alias()   :: atom().\n"
            "-type binary_alias() :: binary().\n",
            #{{test, atom_alias, 0} => #type{module = erlang, type = atom, spec = [], params = []},
              {test, binary_alias, 0} => #type{module = erlang, type = binary, spec = [], params = []}},
            #{})],
    
    {setup,
     fun() -> ok end,
     fun(_) -> file:delete("./assert_types_equal.test") end,
     Tests}.

-define(assert_call_equal(Code, Expected),
        ?_test(begin
                   [Term] = forms:to_abstract(Code),
                   Type = istype_parser:parse_type(?MODULE, Term),
                   ?assertEqual(Expected, Type)
               end)).
parse_primitive_call_test_() ->
    [?assert_call_equal("any().",        #type{module = erlang, type = any,  spec = [], params = []}),
     ?assert_call_equal("none().",       #type{module = erlang, type = none, spec = [], params = []}),
     ?assert_call_equal("pid().",        #type{module = erlang, type = pid,  spec = [], params = []}),
     ?assert_call_equal("port().",       #type{module = erlang, type = port, spec = [], params = []}),
     ?assert_call_equal("reference().",  #type{module = erlang, type = reference, spec = [], params = []}),
     ?assert_call_equal("[].",           #literal{value = {nil, 1}}),
     ?assert_call_equal("atom().",       #type{module = erlang, type = atom, spec = [], params = []}),
     ?assert_call_equal("atom.",         #literal{value = {atom, 1, atom}}),
     ?assert_call_equal("none().",       #type{module = erlang, type = none, spec = [], params = []}),
     ?assert_call_equal("integer().",    #type{module = erlang, type = integer, spec = [], params = []}),
     ?assert_call_equal(
        "list(atom()).",
        #type{module = erlang,
              type   = list,
              spec   = {maybe_empty,
                        #type{module = erlang, type = atom, spec = [], params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_call_equal(
         "maybe_improper_list(atom(), binary()).",
         #type{module = erlang,
               type   = list,
               spec   = {maybe_empty,
                         #type{module = erlang, type = atom,   spec = [], params = []},
                         #type{module = erlang, type = binary, spec = [], params = []}},
               params = []}),
     ?assert_call_equal(
         "nonempty_improper_list(atom(), binary()).",
         #type{module = erlang,
               type   = list,
               spec   = {nonempty,
                         #type{module = erlang, type = atom,   spec = [], params = []},
                         #type{module = erlang, type = binary, spec = [], params = []}},
               params = []}),
     ?assert_call_equal(
         "nonempty_list(atom()).",
         #type{module = erlang,
               type   = list,
               spec   = {nonempty,
                         #type{module = erlang, type = atom, spec = [], params = []},
                         #literal{value = {nil, 1}}},
               params = []}),
     ?assert_call_equal("map().",   #type{module = erlang, type = map,   spec = any,   params = []}),
     ?assert_call_equal("#{}.",     #type{module = erlang, type = map,   spec = empty, params = []}),
     ?assert_call_equal("tuple().", #type{module = erlang, type = tuple, spec = any,   params = []}),
     ?assert_call_equal("{}.",      #type{module = erlang, type = tuple, spec = empty, params = []}),
     ?assert_call_equal(
        "{atom(), binary()}.",
        #type{module = erlang,
              type   = tuple,
              spec   = {2,
                        [{1, #type{module = erlang, type = atom,   spec = [], params = []}},
                         {2, #type{module = erlang, type = binary, spec = [], params = []}}]},
              params = []}),
     ?assert_call_equal("binary().", #type{module = erlang, type = binary, spec = [], params = []}),
     ?assert_call_equal("bitstring().", #type{module = erlang, type = bitstring, spec = [], params = []}),
     ?assert_call_equal("boolean().", #type{module = erlang, type = boolean, spec = [], params = []}),
     ?assert_call_equal(
        "byte().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = 255}},
              params = []}),
     ?assert_call_equal(
        "char().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = 16#10ffff}},
              params = []}),
     ?assert_call_equal(
        "nil().",
        #literal{value = {nil, 1}}),
     ?assert_call_equal(
        "number().",
        #type{module = erlang,
              type   = number,
              spec   = [],
              params = []}),
     ?assert_call_equal(
        "list().",
        #type{module = erlang,
              type   = list,
              spec   = any,
              params = []}),
     ?assert_call_equal(
        "maybe_improper_list().",
        #type{module = erlang,
              type   = list,
              spec   = {maybe_empty,
                        #type{module = erlang, type = any, spec = [], params = []},
                        #type{module = erlang, type = any, spec = [], params = []}},
              params = []}),
     ?assert_call_equal(
        "nonempty_list().",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = any, spec = [], params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_call_equal(
        "string().",
        #type{module = erlang,
              type   = list,
              spec   = {maybe_empty,
                        #type{module = erlang,
                              type   = range,
                              spec   = {#literal{value = 0},
                                        #literal{value = 16#10ffff}},
                              params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_call_equal(
        "nonempty_string().",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang,
                              type   = range,
                              spec   = {#literal{value = 0},
                                        #literal{value = 16#10ffff}},
                              params = []},
                        #literal{value = {nil, 1}}},
              params = []}),
     ?assert_call_equal(
        "iodata().",
        #type{module = erlang,
              type   = union,
              spec   = [#type{module = erlang, type = iolist, spec = [], params = []},
                        #type{module = erlang, type = binary, spec = [], params = []}],
              params = []}),
     ?assert_call_equal(
        "iolist().",
        #type{module = erlang,
              type   = iolist,
              spec   = [],
              params = []}),
     ?assert_call_equal(
        "function().",
        #type{module = erlang,
              type   = 'fun',
              spec   = any,
              params = []}),
     ?assert_call_equal(
        "module().",
        #type{module = erlang,
              type   = atom,
              spec   = [],
              params = []}),
     ?assert_call_equal(
        "mfa().",
        #type{module = erlang,
              type   = tuple,
              spec   = {3,
                        [{1, #type{module = erlang, type = atom, spec = [], params = []}},
                         {2, #type{module = erlang, type = atom, spec = [], params = []}},
                         {3, #type{module = erlang,
                                   type   = range,
                                   spec   = {#literal{value = 0},
                                             #literal{value = 255}},
                                   params = []}}]},
              params = []}),
     ?assert_call_equal(
        "arity().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = 255}},
              params = []}),
     ?assert_call_equal(
        "identifier().",
        #type{module = erlang,
              type   = union,
              spec   = [#type{module = erlang, type = pid, spec = [], params = []},
                        #type{module = erlang, type = port, spec = [], params = []},
                        #type{module = erlang, type = reference, spec = [], params = []}],
              params = []}),
     ?assert_call_equal(
        "node().",
        #type{module = erlang,
              type   = atom,
              spec   = [],
              params = []}),
     ?assert_call_equal(
        "timeout().",
        #type{module = erlang,
              type   = union,
              spec   = [#literal{value = {atom, 1, infinity}},
                        #type{module = erlang,
                              type   = range,
                              spec   = {#literal{value = 0},
                                        #literal{value = undefined}},
                              params = []}],
              params = []}),
     ?assert_call_equal(
        "no_return().",
        #type{module = erlang,
              type   = none,
              spec   = [],
              params = []}),
     ?assert_call_equal(
        "non_neg_integer().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 0},
                        #literal{value = undefined}},
              params = []}),
     ?assert_call_equal(
        "pos_integer().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = 1},
                        #literal{value = undefined}},
              params = []}),
     ?assert_call_equal(
        "neg_integer().",
        #type{module = erlang,
              type   = range,
              spec   = {#literal{value = undefined},
                        #literal{value = -1}},
              params = []}),
     ?assert_call_equal(
        "nonempty_maybe_improper_list().",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = any, spec = [], params = []},
                        #type{module = erlang, type = any, spec = [], params = []}},
              params = []}),
     ?assert_call_equal(
        "nonempty_maybe_improper_list(atom(), atom()).",
        #type{module = erlang,
              type   = list,
              spec   = {nonempty,
                        #type{module = erlang, type = atom, spec = [], params = []},
                        #type{module = erlang, type = atom, spec = [], params = []}},
              params = []}),
     ?assert_call_equal(
        "#record{}.",
        #type{module = erlang,
              type   = record,
              spec   = {record, []},
              params = []}),
     ?assert_call_equal(
        "#record{a = binary()}.",
        #type{module = erlang,
              type   = record,
              spec   = {record, [{a, #type{module = erlang, type = binary, spec = [], params = []}}]},
              params = []})].
    














