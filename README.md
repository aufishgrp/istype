# istype [![Hex.pm](https://img.shields.io/hexpm/v/istype.svg)](https://hex.pm/packages/istype)

Library that provides parse transforms to enhance Erlang type checking and conversion.

## Usage
1. Add the package to your rebar.config

   ```
   {deps, [
       {istype, "0.1.1"}
   ]}.
   ```
2. Add the parse transform to your rebar.config

   ```
   {erl_opts, [
       {parse_transform, istype_transform}
   ]}.
   ```
3. Use the transforms as documented in their sections below.

## istype
Transform that generates guard friendly type checking statements based on the typespec given. This removes the need for the user to implement and maintain code for type validation.

### Value istype

```
-type timeout() :: integer() || infinity.

%% Before transform
istype(Value, timeout()).

%% After transform
is_integer(Value) orelse Value =:= infinity.
```

The expression generated to validate type is generated in an in order manner.

```
-type timeout0() :: integer() || infinity.
-type timeout1() :: infinity || integer().

%% Before transform
istype(Value0, timeout0()).

istype(Value1, timeout1()).

%% After transform
is_integer(Value0) orelse Value0 =:= infinity.

Value1 =:= infinity orelse is_integer(Value1).
```

### Expression istype

```
-type timeout() :: integer() || infinity.

%% Before transform
istype(expression(), timeout()).

%% After transform
begin
    __IsType_X = expression(),
    is_integer(__IsType_X) orelse __IsType_X =:= infinity
end.
```

When an expression is provided to istype, it is evaluated prior to any checks being processed. This prevents side effects from accidentally processing multiple times. 

```
-type timeout() :: integer() || infinity.

%% Before transform
istype(size(Value), timeout()).

%% After transform
is_integer(size(Value)) orelse size(Value) =:= infinity.
```

Exceptions are made for any BIF that would be allowed in a guard statement. This is to allow the check to remain an allowable guard.

### Tuple istype

```
-type tuple0() :: {atom}.

%% Before transform
istype(Value, tuple0()).

%% After transform
is_tuple(Value) andalso size(Value) =:= 1 andalso element(1, Value) =:= atom.
```

When a type specifies a tuple a specific tuple format as a primitive the arity and field types are also checked.

```
%% Before transform
istype(Value, tuple()).

%% After transform
is_tuple(Value).
```

The `tuple()` type is treated as any tuple.

### Record istype

```
-record(record0, {a :: integer(), b}).

-type record0() :: #record0{}.

%% Before transform
istype(Value, record0()).

%% After transform
is_tuple(Value) andalso size(Value) =:= 3 andalso is_integer(Value#record0.a).
```

As with tuples records are checked against arity and field types.

### Nested types
```
-type timeout() :: integer() | infinity.
-type tuple0() :: {timeout()}.

%% Before transform
istype(Value, tuple0()).

%% After transform
is_tuple(Value) andalso size(Value) =:= 1 andalso (is_integer(element(1, Value)) orelse
                                                   element(1, Value) =:= infinity).
```

## asserttype
Transform that asserts that the value is the specified type. Functionally the same as `true = istype(Value, type()).`

## totype
Transform that enables conversion to custom types. Generates a call to totype:convert/3 with the nested type specs needed for type conversion.

### Value totype
```
-type timeout() :: integer().

1 = totype("1", timeout()).
1 = totype(1.0, timeout()).
1 = totype(<<"1">>, timeout()).
```

Type conversion looks at the input given for the target type and determines how to convert.

```
-type type0() :: binary() | list().
-type type1() :: list() | binary().

<<"1">> = totype(1, type0()).
"1" = totype(1, type1()).
```

When converting to a type that is the union of multiple types the first valid conversion is returned.

### Records totype
```
-record(record0, {a = 0 :: integer()}).
-type record0() :: #record0{}.

#record0{a = 1} = totype([{a, 1}], record0()).
#record0{a = 1} = totype([{"a", "1"}], record0()).
#record0{a = 1} = totype(#{<<"a">> => <<"1">>}], record0()).
```

Records can be generated from maps and proplists. If a map or list contains a key that is not present in the record that value is dropped silently.

```
-record(record0, {a = 0 :: integer()}).
-record(record1, {a = <<"">> :: binary()}).

-type record0() :: #record0{}.
-type record1() :: #record1{}.

#record0{a = 1} = totype(#record0{a = "1"}, record0()).
#record1{a = <<"1">>} = totype(#record0{a = "1"}, record1()).
```

Records can be converted to other records. As with lists and maps only the shared keys are copied.

```
-record(record0, {a = 0 :: integer()}).

#{a => 2} = totype(#record{a = 2}, map()).
[{a, 2}] = totype(#record{a = 2}, list()).
```

Records can also be converted into maps and proplists.
