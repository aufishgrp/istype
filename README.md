# istype [![Travis-CI.org](https://travis-ci.org/aufishgrp/istype.svg?branch=dev)](https://travis-ci.org/aufishgrp/istype/branches) [![Hex.pm](https://img.shields.io/hexpm/v/istype.svg)](https://hex.pm/packages/istype)

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
Transform that generates guard friendly type checking statements based on the typespec given. This removes the need for the user to implement and maintain code for type validation. A boolean result is always provided.

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

When an expression is provided to istype, it is evaluated prior to any checks being processed. This prevents side effects from being processed multiple times. 

```
-type timeout() :: integer() || infinity.

%% Before transform
istype(size(Value), timeout()).

%% After transform
is_integer(size(Value)) orelse size(Value) =:= infinity.
```

Exceptions are made for any BIF that would be allowed in a guard statement. This is to allow the check to remain an allowable guard.

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
Transform that enables conversion to any type. Returns a value of the specified type or throws an error of the format {istype\_conversion, Value, Type, Reason}. For convenience we'll define this as such.

```
-type conversion_error() :: {istype_conversion, Value, Type, Reason}
```

If the error is thrown because a nested component could not be converted then Reason is also a conversion\_error(). Reason may be many levels deep in this manner.


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

## As it applies to the Erlang typespec

How istype and totype handle the types defined by the [Erlang typespec](http://erlang.org/doc/reference_manual/typespec.html).

If any of the type subsections do not include an istype or totype subsection it is assumed that any comments are obvious and unnecessary.

##### istype
The typespec for istype is expected to be a call to the type in question or a literal value. 

```
%% Before
istype(Value, atom()),
%% Ater
is_atom(Value),

%% Before
istype(Value, atom),
%% After
Value =:= atom.
```

There are a few exceptions to this rule, specifically for complex types such as maps, tuples, and records.

It should also be pointed out that not all patterns you use in type specs can be used as the type spec to istype. This is due to the nature of the Erlang parser. Bitstrings being the most notable of this group. In these cases it's necessary to specify a custom type to use within istype.

##### totype
The typespec for totype is expected to be a call to the type in question or a literal value. 

```
totype(Value, atom()),
totype(Value, atom),
```

There are a few exceptions to this rule, specifically for complex types such as maps, tuples, and records.

It should also be pointed out that not all patterns you use in type specs can be used as the type spec to totype. This is due to the nature of the Erlang parser. Bitstrings being the most notable of this group. In these cases it's necessary to specify a custom type to use within totype.

### any()
By definition all values fall within the any() type.

##### istype
Always returns true; this can cause compile warnings if used in a guard that may expect a non true value.

##### totype
Always returns the value to be converted.

### none()
By definition no values fall within the none() type.

##### istype
Always returns false; this can cause compile warnings if used in a guard that may expect a non false value.

##### totype
Always throws a conversion\_error().

### pid()
##### totype
Supports conversion from the following types

* pid()
* list()
* binary()

### port()
##### totype
Supports conversion from the following types

* port()
* list()
* binary()

### reference()
##### totype
Supports conversion from the following types

* reference()
* list()
* binary()

### []
##### totype
Supports conversion from the following types

* list()
* binary()

The nil type specifies a specific value. If the converted value is not of nil() typing a conversion\_error() is thrown.

### Atom
```
Atom :: atom()
      | Erlang_Atom %% 'foo', 'bar', ...
```
#### atom()
##### totype
Supports conversion from the following types

* atom()
* list()
* binary()

#### ERLANG_ATOM
##### istype
Returns true when value is the literal atom specified as the type.
##### totype
Supports conversion from the following types

* atom()
* list()
* binary()

Returns the anticipated atom if value can be converted to it, throws a conversion\_error() otherwise.

### Bitstring
```
Bitstring :: <<>>
           | <<_:M>>        %% M is a positive integer
           | <<_:_*N>>      %% N is a positive integer
           | <<_:M, _:_*N>>
```

Unlike other types the notation to represent a bitstring's format is invalid within code. Bitstring formats must be masked by a user type.

##### totype
Supports conversion from the following types

* atom()
* Bitstring
* binary()
* list()

Conversion to Bitstring types converts the value into a bitstring and then checks that it's of a valid length. If the length is invalid a istype\_conversion error is thrown. No attempts are made to pad the resulting bitstring to an appropriate length.

### float()
##### totype
Supports conversion from the following types

* float()
* Integer
* binary()
* list()

### Fun
Converting to a fun is not supported.

### Integer
```
Integer :: integer()
         | Erlang_Integer                 %% ..., -1, 0, 1, ... 42 ...
         | Erlang_Integer..Erlang_Integer %% specifies an integer range
```
#### integer()
##### totype
Supports conversion from the following types

* float()
* Integer
* binary()
* list()

#### Integer
Same as integer() but Value must be, or convert to, the specified Integer.

#### Integer..Integer
Same as Integer but Value may be any of the values between, inclusive of, the specified Integers.

### List
```
List :: list(Type)                           %% Proper list ([]-terminated)
      | maybe_improper_list(Type1, Type2)    %% Type1=contents, Type2=termination
      | nonempty_improper_list(Type1, Type2) %% Type1 and Type2 as above
      | nonempty_list(Type)                  %% Proper non-empty list
```

Internally all list types are converted into a generalized format of

```
list(empty|non_empty, ValueType(), TerminatorType)
```

##### istype
Istype is not always guard safe when examining lists. When possible the transform will use a combination of is_list/1 and length/1 to validate that the provided value is a list and is non empty. When the type spec specifies specific types for the values and a specific terminator other than nil() each element of the list must then be examined. This cannot be done in a guard safe manner.

##### totype
Supports conversion from the following types

* pid()
* port()
* reference()
* Atom
* Bitstring
* float()
* Integer
* Map
* Tuple
* Record

### Map
```
Map :: map()                                 %% denotes a map of any size
     | #{}                                   %% denotes the empty map
     | #{AssociationList}
     
AssociationList :: Association
                 | Association, AssociationList
                   
Association :: Type := Type                  %% denotes a mandatory association
             | Type => Type                  %% denotes an optional association
```

##### istype
Evaluating istype against maps is guard safe for map() and #{} only.

Maps are seen as valid when all keys match against a key typing and all associated values have the matching value typing. Additionally all mandatory fields must be present and no non-specified fields may be present.

##### totype
Supports conversion from the following types

* float()
* integer()
* map()
* tuple()
* record()
* list()
* binary()

