# Syntax

## S-expression grammar

This is the S-expression grammar of the MeTTa language. The program is
consisted of the atoms prefixed or not prefixed by the `!` sign.

```
METTA ::= { [ '!', [ DELIM ] ], ATOM, [ DELIM ] } ;

ATOM ::= SYMBOL | VARIABLE | GROUNDED | EXPRESSION ;
SYMBOL ::= WORD;
WORD ::= ( CHAR | '#' ), { CHAR | '"' | '#' } ;
VARIABLE ::= '$', ( CHAR | '"' ), { CHAR | '"' } ;
GROUNDED ::= STRING | WORD ;
EXPRESSION ::= '(', { ATOM, [ DELIM ] }, ')' ;

STRING ::= '"', { CHAR | WHITESPACE | '#' | '(' | ')' | ';' }, '"' ;
COMMENT ::= ';', { CHAR | ' ' | '\t' | '"' | '#' | '(' | ')' | ';' } ! EOL ;
DELIM ::= { WHITESPACE | COMMENT } ;

CHAR ::= <any character except WHITESPACE and ( '"' | '#' | '(' | ')' | ';' )> ;
WHITESPACE ::= ' ' | '\t' | EOL ;
EOL ::=  '\r' | '\n' ;
```

(*) Symbol atom can start with the `!` character. For example `!name` is a
valid symbol and not an atom prefixed by `!`. This is an artifact of the HE
parser evolution process and not a strong requirement.

(*) The `#` character is used internally by HE to represent the automatically
generated variables. It is the reason why `#` is reserved and cannot be used
inside a variable name.

(*) Whitespace character is any character for which `char.is_whitespace()`
method from the Rust standard library returns true.

The symbol and grounded atoms are very similar from the grammar perspective.
The difference is in the way how they are represented in memory. A symbol can
be considered an id. A grounded atom on the other hand may have state and
usually has some natural representation in the host language (i.e. Rust, Python
or other programming language).

The grounded atoms are constructed from the `WORD` or `STRING` grammar tokens
by a tokenizer. Tokenizer is a collection of the pairs `(<token regexp>,
<constructor function>)`. If the `WORD` or `STRING` token is matched by one of
the `<token regexp>` from the tokenizer then a grounded atom is constructed by
`<constructor function>`. `<constructor function>` gets the matched token as an
argument. For instance it is possible instantiating the integer grounded atoms
by adding the pair `([0-9]+, <int parser>)` into the tokenizer.

## Special expressions

This section lists atoms with some special meaning in the interpreter.  The
expressions listed below are not the only expressions possible when the special
atoms are used. For example having the `(= ...)` expression with more than two
arguments is absolutely normal. In such case the interpreter will not recognize
it as a function definition but the expression still can be the part of the
knowledge base.

In general the symbols listed below are similar to the reserved words in the
other languages. In MeTTa the programmer still can use these symbols in an
unusual context but it should be noted that the interpreter has a special
meaning for them.

### Function expression

The `=` atom is used to define a function expression in the following form:
```
(= <call recognition template> <body template>)
```

For example the `if` function can be defined as following:
```
(= (if True $then $else) $then)
(= (if False $then $else) $else)
```

### Type assignment

The `:` atom is used to define the type of the atom:
```
(: <atom> <type>)
```

For example the `if` function could have the following type definition:
```
(: if (-> Bool Atom Atom $t))
```

### Function type

The `->` atom is used to introduce a type of a function:
```
(-> arg1_type arg2_type ... argN_type ret_type)
```

For example the type of the `if` function:
```
(-> Bool Atom Atom $t)
```

### Elementary types

There are the number of symbols which are used as the basic types:
- `Type` - the type of any type
- `%Undefined%` - the unknown type
- `Atom` - the type of any atom
- `Symbol` - the type of the symbol atom
- `Variable` - the type of the variable atom
- `Expression` - the type of the expression atom
- `Grounded` - the type of the grounded atom

All of them except the `Type` affect the order of the expression evaluation.
Last four types correspond to the types of the atoms defined by the grammar.
These types plus `Atom` are referred below as meta-types. `Atom` is a special
meta-type which matches any atom and used for changing the order of evaluation.


### Special function results

- `Empty` - the function doesn't return any result, it is different from the
  void or unit result in other languages
- `NotReducible` - returns the unchanged function call instead
- `Error` - the error result constructor

The `Error` expression has the following format:
```
(Error <atom with error> <error message>)
```
where `<error message>` is either a string or a symbol or one of the following:
- `StackOverflow` - returned by the interpreter when the stack depth is
  restricted and maximum depth is reached
- `NoReturn` - this error is reserved by the minimal MeTTa interpreter and
  should not be returned if program doesn't have `(function ...)` minimal MeTTa
  blocks (see [minimal MeTTa documentation](./minimal-metta.md#functionreturn)
  for more information)
- `IncorrectNumberOfArguments` - the number of argument in the call doesn't
  equal to the number of parameters of the called function
- `(BadArgType <arg position> <expected type> <actual type>)` - the argument
  type error
- `(BadType <expected type> <actual type>)` - the type cast error

(*) `ErrorType` is a type of the error atom which is returned by the `Error`
type constructor. This symbol has no special meaning in HE. On the other hand
it could be used to introduce custom type constructors. This could be
implemented by making an interpreter treating any expression which has
`ErrorType` type as an error.

### Minimal MeTTa instructions

The minimal MeTTa is an attempt to create an assembly language for MeTTa.
Minimal MeTTa related atoms are listed here because while they are not a part
of the standard library they are still recognized and evaluated by the MeTTa
interpreter. Full documentation on minimal MeTTa can be found at
[minimal-metta.md](./minimal-metta.md).

(*) It is possible to implement MeTTa interpreter in minimal MeTTa and HE
interpreter is implemented in this way. Implementing minimal MeTTa interpreter
in pure MeTTa is not possible. But on the other hand one could implement
minimal MeTTa as a MeTTa library containing the corresponding grounded
functions. Some internal APIs of HE should be changed to make this doable. Main
reason is that the grounded functions don't have access to the context of the
MeTTa interpreter, for instance to the current working atomspace.

Minimal MeTTa instructions:
- `(eval <atom to evaluate>)` - makes one step of the evaluation
- `(evalc <atom to evaluate> <context space>)` - makes one step of the
  evaluation in the context of the passed space
- `(chain <atom> <var> <template>)` -  interpret `<atom>` and substitute
  `<var>` in `<template>` by the result of the interpretation
- `(unify <atom> <pattern> <then> <else>)` - unify `<atom>` argument with
  `<pattern>` argument and return `<then>` argument if the unification is
  successful and `<else>` argument otherwise.
- `(decons-atom <expression>)` - return the head and the tail of the passed
  expression
- `(cons-atom <head> <tail>)` - return the expression constructed from `<head>`
  and `<tail>`
- `(function <body>)` - evaluate `<body>` until `(return <atom>)` is evaluated
- `(return <atom>)` - finish the evaluation of an outer function with `<atom>`
  as the result
- `(collapse-bind <atom>)` - interpret `<atom>` and return a tuple which
  contains all of the results
- `(superpose-bind <result of collapse-bind>)` - continue the process of the
  interpretation from the moment where `collapse-bind` stopped
- `(metta <atom> <type> <space>)` - evaluate `<atom>` in MeTTa interpreter
  using `<space>` as a context and expecting result with `<type>`
- `(context-space)` - return the space which is used by the interpreter
- `(call-native <function name> <pointer to the function> <arguments>)` - call
  the passed Rust function with the passed arguments

(*) `call-native` instruction cannot be called from a MeTTa program, but it can
be returned by a grounded function for the further evaluation.

# Interpretation

MeTTa program is interpreted atom by atom. If an atom is not prefixed by `!` it
is added into the atomspace of the top module. Hierarchy of modules is
explained later. For now one can count that there is no atomspaces except the
top module's atomspace. When an atom is prefixed by `!` sign it is evaluated
and the result of the evaluation is returned to the user. The result is not
added into the atomspace. The "atom by atom" evaluation means that each atom
sees the effects which were applied by the preceding atom evaluations or
atomspace modifications.

The evaluation of the atom is a call of the `metta` symbol with four arguments:
atom to be evaluated, expected type, context space, and variable bindings. The
expected type of the evaluated atom is `%Undefined%` because we don't have the
syntax for specifying it. The variable bindings are empty at the beginning of
the evaluation.

`metta` symbol itself is a part of the interpreter implementation. It allows
calling the interpreter within the MeTTa program itself. Thus the algorithm of
the evaluation of the `metta` symbol is the algorithm of the interpreter
implementation. This algorithm is listed below in [Evaluation section](#evaluation).
This algorithm is not equal to the Rust implementation of the HE but in many
aspects it is similar and usually one can see the correspondence with the Rust
code.

The algorithm is presented in a form of a program on some Python-like pseudo
programming language.  `<...>` notation is used to embed the statements which
depend on the data structures used because this details are not the part of the
specification. The names of the variables in the pseudo code are started with
`$` sign to differentiate them from MeTTa symbols which are written in the same
way they are used in MeTTa programs. Also there is a `~` operator which
designates the matching of the atom variable with some atom pattern. This
operator is used to make the specification shorter.

Two special functions are not specified: `match_atoms` and `merge_bindings`.
They will be specified in the following sections. For now one can rely on
intuitive understanding how two-side unification and bindings merging works.

## Evaluation

### Evaluate atom (metta)

```
Input:
- $atom - atom to be evaluated
- $type - expected type of the result
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [(Atom, Bindings)]

$metatype =  <meta-type of the $atom>
if $atom == Empty or $atom ~ (Error ...):
    return [($atom, $bindings)]
elif $type == Atom or $type == $metatype or $metatype == Variable:
    return [(atom, bindings)]
elif $metatype == Expression and <$atom is evaluated already>:
    return [(atom, bindings)]
elif $metatype == Symbol or $metatype == Grounded or $atom == ():
    return type_cast(atom, bindings, type, space)
else:
    $results = interpret_expression($atom, $type, $space, $bindings)
    $error = filter(lambda $a: $a ~ (Error ...), $results)
    $success = filter(lambda $a: not($a ~ (Error ...)), $results)
    if len($success) > 0:
        for $a in $success:
            if <$a is expression>:
                # this is an optimization step
                <mark $a as evaluated>
        return $success
    else:
        return $error
```

### Cast types (type_cast)

```
Input:
- $atom - atom to cast
- $type - type to cast to
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [(Atom, Bindings)]

$types = <list of the types of the $atom from the $space>
$no_match = []
for $t in $types:
    $matches = match_types($t, $type, $bindings):
    if $matches == []:
        $no_match += [$t]
    else:
        return [($atom, $m) for $m in $matches]
return [((Error $atom (BadType $type $t)), $bindings) for $t in $no_match]
```

### Match types (match_types)

```
Input:
- $type1 - first type
- $type2 - second type
- $bindings - current variable bindings

Output:
- [Bindings]

if $type1 == %Undefined% or $type1 == Atom
    or $type2 == %Undefined% or $type2 == Atom:
        return [$bindings]

return match_atoms($type1, $type2)
```

### Interpret expression (interpret_expression)

```
Input:
- $atom - atom to be evaluated
- $type - expected type of the result
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [(Atom, Bindings)]

$op = <first item of the $atom tuple>
if <$op is an atom with incorrect type>:
    # it may happen if $op is a function call
    return <list of (Error $op (BadArgType ...)) for each erroneous type found>
$actual_types = <list of the types of the $op>

$errors = []
for $f in <items from $actual_types which are valid function types>:
    match check_if_function_type_is_applicable($atom, $f, $type, $space, $bindings):
        case Err($errs):
            $errors += [($e, $bindings) for $e in $errs]
        case Ok($succs):
            $ret_type = <return type of $f>
            if $ret_type == Expression:
                # it is to prevent Expression working like Atom return type
                $ret_type = %Undefined%
            $result = []
            for $b in $succs:
                for ($a, $b) in interpret_function($atom, $f, $type, $space, $b):
                    result += metta_call($a, $ret_type, $space, $b)
                return $result

$tuples = []
if <$actual_types contains non function types or %Undefined% type>:
    for ($a, $b) in interpret_tuple($atom, $space, $bindings):
        $tuples += metta_call($a, $type, $space, $b)

return $tuples + $errors
```

### Interpret tuple (interpret_tuple)

```
Input:
- $atom - atom to be evaluated
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [(Atom, Bindings)]

$result = []
$head = <head of the $atom tuple>
$tail = <tail of the $atom tuple>
for ($h, $hb) in metta($head, %Undefined%, $space, $bindings):
    if $h == Empty or $h ~ (Error ...):
        $result += [($h, $hb)]
    else:
        for ($t, $tb) in interpret_tuple($tail, $space, $hb):
            if $t == Empty or $t ~ (Error ...):
                $result += [($t, $tb)]
            else:
                $result += [(<tuple with head $h and tail $t>, $tb)]
return $result
```

### Check if function type is applicable (check_if_function_type_is_applicable)

```
Input:
- $atom - function call to check
- $func_type - function type
- $expected_type - expected return type
- $space - context atomspace
- $bindings - current variable bindings

Output:
- Err([Atom]) | Ok([Bindings])

if <number of arguments in $func_type> != <number of arguments in $atom>:
    return [Err((Error $atom IncorrectNumberOfArguments), $bindings)]

$errors = []
$results = [$bindings]
for $idx in range(1, len($atom)):
    $arg = $atom[$idx]
    $next = []
    for $r in $results:
        $check = check_argument_type($arg, $func_type[$idx], $space, $r)
        for $c in $check:
            if $c ~ Err($t):
                $errors += [(Error $atom (BadArgType ($idx - 1) $func_type[$idx] $t))]
            if $c ~ Ok($b):
                $next += [$b]
    $results = $next

$next = []
for $r in $results:
    $matched = match_types($expected_type, $func_type.last, $r)
    if matched == []:
        $errors += [(Error $atom (BadType $expected_type $func_type.last))]
    else:
        $next += $matched
$results = $next

if len($results) == 0:
    return Err($errors)
else:
    return Ok($results)
```

### Check argument type (check_argument_type)

```
Input:
- $argument - argument to check
- $expected_type - expected type
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [Err(Type) | Ok(Bindings)]

$actual_types = <list of the types of the $argument from the $space>
$result = []
for $t in $actual_types:
    $matched = match_types($expected_type, $t, $bindings)
    if $matches == []:
        $result += [Err($t)]
    else:
        $result += [Ok($m) for $m in $matches]
return $result
```

### Interpret function (interpret_function)
```
Input:
- $atom - atom to be evaluated
- $op_type - type of the function
- $return_type - expected return type
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [(Atom, Bindings)]

$result = []
$op = <head of the $atom tuple>
$args = <tail of the $atom tuple>
$arg_types = <list of the argument types extracted from $op_type>
for ($h, $hb) in metta($op, $op_type, $space, $bindings):
    if $h == Empty or $h ~ (Error ...):
        $result += [($h, $hb)]
    else:
        for ($t, $tb) in interpret_args($args, $arg_types, $space, $hb):
            if $t == Empty or $t ~ (Error ...):
                $result += [($t, $tb)]
            else:
                $result += [(<tuple with head $h and tail $t>, $tb)]
return $result
```

### Interpret arguments (interpret_args)

```
Input:
- $args - args to be evaluated
- $types - types of the args
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [(Atom, Bindings)]

$result = []
$atom = <head of the $args tuple>
$args_tail = <tail of the $args tuple>
$type = <head of the $types tuple>
$types_tail = <tail of the $types tuple>
for ($h, $hb) in metta($atom, $type, $space, $bindings):
    if ($h == Empty or $h ~ (Error ...)) and $h != $atom:
        $result += [($h, $hb)]
    else:
        for ($t, $tb) in interpret_args($args_tail, $types_tail, $space, $hb):
            if $t == Empty or $t ~ (Error ...):
                $result += [($t, $tb)]
            else:
                $result += [(<tuple with head $h and tail $t>, $tb)]
return $result
```

### Call MeTTa expression (metta_call)

```
Input:
- $atom - atom to be evaluated
- $type - expected type of the result
- $space - context atomspace
- $bindings - current variable bindings

Output:
- [(Atom, Bindings)]

if $atom ~ (Error ...):
    return [($atom, $bindings)]

$op = <head atom of the $atom expression>
$args = <tail of the $atom expression>
$results = []
if <$op is executable grounded atom>:
    match <call $op native function passing $args as arguments>:
        case Ok($results):
            $results = [metta($r, $type, $space, $mb) for ($r, $rb) in $results for $mb in merge_bindings($rb, $bindings)]
        case RuntimeError($message):
            return [(Error $atom <symbol atom containing $message>)]
        case NoReduce:
            return [($atom, $bindings)]
        case IncorrectArgument:
            return [($atom, $bindings)]
else:
    for $rb in query($space, (= $atom $X)):
        for $mb in merge_bindings($rb, $bindings):
            if not(<$mb has loop bindings>) and <$mb contains value for the $X variable>:
                $x = <get value of the $X variable from $mb>
                $results += [metta($x, $type, $space, $mb)]

if len($results) == 0:
    return [(Empty, $bindings)]
else:
    return $results
```

## Matching

This section explains the atom matching algorithm. The result of matching is
list of variable binding sets. If two atoms matched don't contain grounded
atoms with custom matching procedure then the result of matching is exactly one
binding set (because two atoms are either equal or not). But in general case
custom matching is possible and the result may be a list of binding sets.

Each binding is a set of two kinds of relations. First kind of relation is
assigning a value to a variable. This relation is designated by `<-` arrow. For
example `$x <- SomeValue` means the symbol atom `SomeValue` is assigned to the
variable `$x`.  Second kind of relation is equality of two variables. This
relation is designated by `=` sign. For eample `$x = $y` means variables `$x`
and `$y` are equal. The equality means variables have equal or matchable
values. The full set of bindings is designated using curly braces with
relations inside listed using comma. For example: `{ $x <- SomeValue, $x = $y
}`. The order of relations doesn't matter.

For a sake of simplicity I used operations `+` and `-` in algorithms below to
explain how binding set is modified. For example `$bindings - { $b <- $b_value
} + { $a = $b }` means one need remove relation `$b <- $b_value` from $bindings
and add relation `$a = $b`.

### Match atoms (match_atoms)

```
Input:
- $left - atom to be evaluated
- $right - expected type of the result

Output:
- [Bindings]

$ml = <meta type of the $left atom>
$mr = <meta type of the $right atom>
$result = [{}] 

if $ml == Symbol and $mr == Symbol and $left == $right:
    $result = [{}]
elif $ml == Variable and $mr == Variable:
    $result = [{ $left = $right }]
elif $ml == Variable:
    $result = [{ $left <- $right }]
elif $mr == Variable:
    $result = [{ $right <- $left }]
elif $ml == Expression and $mr == Expression and len($left) == len($right):
    for $i in range(0, len($left)):
        $sub = match_atoms($left[$i], $right[$i])
        $next = []
        for $a in $result:
            for $b in $sub:
                $next += merge_bindings($a, $b)
        $result = $next
elif $ml == Grounded and <$left has custom matching implementation>:
    $result = <call $left custom matching on $right>
elif $mr == Grounded and <$right has custom matching implementation>:
    $result = <call $right custom matching on $left>
elif $ml == Grounded and $mr == Grounded:
    $result = [{}]
else:
    $result = []

return filter(lambda $b: <$b doesn't have variable loops>, $results)
```

### Merge bindings (merge_bindings)

```
Input:
- $left - variable bindings
- $right - variable bindings

Output:
- [Bindings]

$result = [$left]
for $rel in <set of "assign value to var" or "vars are equal" relations of $right>:
    if <$rel is "assign value $val to var $var" relation>:
        $result = [ add_var_binding($r, $var, $val) for $r in $result]
    if <$rel is "var $a is equal to var $b">:
        $result = [ add_var_equality($r, $a, $b) for $r in $result]
return $result
```

### Add variable binding to binding set (add_var_binding)

```
Input:
- $bindings - variable bindings
- $var - variable atom
- $val - value atom

Output:
- [Bindings]

$prev = <value of $var in $bindings or None>
if $prev is None:
    return [$bindings + { $var <- $val }]
elif $val == $prev:
    return [$bindings]
else:
    $match = match_atoms($prev, $val)
    for $b in $match:
        $result += merge_bindings($bindings, $b)
    return $result
```

### Add variable equality to binding set (add_var_equality)

```
Input:
- $bindings - variable bindings
- $a - first variable atom
- $b - second variable atom

Output:
- [Bindings]

$a_value = <value of $a in $bindings or None>
$b_value = <value of $b in $bindings or None>

if $a_value is None or $b_value is None or $a_value == $b_value:
    return [$bindings - { $b <- $b_value } + { $a = $b }]
else:
    $result = []
    $match = match_atoms($a_value, $b_value)
    for $b in $match:
        $result += merge_bindings($bindings, $b)
    return $result
```
