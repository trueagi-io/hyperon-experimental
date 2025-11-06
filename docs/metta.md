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

All of them except the `Type` affect the order of the expression evaluation
(TODO add link).

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
- `(BadArgType <arg position> <expected type> <actual type>)`- the argument
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

# Evaluation

MeTTa program is evaluated atom by atom. If atom is not prefixed by '!' it is
added into the top atomspace.
