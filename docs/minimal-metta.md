This document describes the minimal set of embedded MeTTa instructions which is
designed to write the complete MeTTa interpreter in MeTTa. Current version of the
document includes improvements which were added after experimenting with the
first version of such an interpreter. It is not a final version and some
directions of the future work is explained at the end of the document.

# Minimal instruction set

## Interpreter state

The MeTTa interpreter evaluates an atom passed as an input. It evaluates it
step by step executing a single instruction on each step. In order to do that
the interpreter needs a context which is wider than the atom itself. The
context also includes:
- an atomspace which contains the knowledge which drives the evaluation of the
  expressions;
- bindings of the variables which are used to evaluate expressions; the
  bindings are empty at the beginning (see [Explicit atomspace variable
  bindings](#explicit-atomspace-variable-bindings)).

Each step of interpretation inputs and outputs a list of pairs (`<atom>`,
`<bindings>`) which is called interpretation plan. Each pair in the plan
represents one possible way of interpreting the original atom or possible
branch of the evaluation. Interpreter doesn't select one of them for further
processing. It continues interpreting all of the branches in parallel. Below
this is called non-deterministic evaluation.

One step of the interpretation is an execution of a single instruction from a
plan. An interpreter extracts atom and bindings from the plan and evaluates the
atom. The result of the operation is a set of pairs (`<atom>`, `<bindings>`).
Bindings of the result are merged with the previous bindings. Merge operation
can also bring more than one result. Each such result is added as a separate
pair into a result set. Finally all results from result set are added into the
plan and step finishes.

Here we suppose that on the top level the plan contains only the instructions
from the minimal set. If an instruction returns the atom which is not from the
minimal set it is not interpreted further and returned as a part of the final
result. Thus only the instructions of the minimal set are considered a code
other atoms are considered a data.

## Evaluation order

MeTTa implements the applicative evaluation order by default, arguments are
evaluated before they are passed to the function. User can change this order
using special meta-types as the types of the arguments. Minimal MeTTa
operations don't rely on types and minimal MeTTa uses the fixed normal
evaluation order, arguments are passed to the function without evaluation. But
there is a [chain](#chain) operation which can be used to evaluate an argument
before passing it. Thus `chain` can be used to change evaluation order in MeTTa
interpreter.

## Error/Empty/NotReducible/()

There are atoms which can be returned to designate a special situation in a code:
- `(Error <atom> <message>)` means the interpretation is finished with error;
- `Empty` means the corresponding branch of the evaluation returned no results,
  such result is not returned among other results when interpreting is
  finished;
- `NotReducible` can be returned by `eval` in order to designate that function
  can not be reduced further; for example it can happen when code tries to call
  a type constructor (which has no definition), partially defined function
  (with argument values which are not handled), or grounded function which
  returns `NotReducible` explicitly; this atom is introduced to separate the
  situations when atom should be returned "as is" from `Empty` when atom should
  be removed from results;
- Empty expression `()` is an instance of the unit type which is mainly used by
  functions with side effects which has no meaningful value to return.

These atoms are not interpreted further as they are not a part of the minimal
set of instructions and considered a data.

## eval

`(eval <atom>)` is a first instruction which evaluates an atom passed as an
argument. Evaluation is different for the grounded function calls (the
expression with a grounded atom on a first position) and pure MeTTa
expressions. For the pure MeTTa expression the interpreter searches the `(=
<atom> <var>)` expression in the atomspace. The found values of the `<var>` are
the result of evaluation. Execution of the grounded atom leads to the call of
the foreign function passing the tail of the expression as arguments. For
example `(+ 1 2)` calls the implementation of addition with `1` and `2` as
arguments.  The list of atoms returned by the grounded function is a result of
the evaluation in this case. A grounded function can have side effects as well.
In both cases bindings of the `eval`'s argument are merged to the bindings of
the result.

Atomspace search can bring the list of results which is empty. When search
returns no results then `NotReducible` atom is a result of the instruction.
Grounded function can return a list of atoms, empty result,
`ExecError::Runtime(<message>)` or `ExecError::NoReduce` result. The result of
the instruction for a special values are the following:
- `ExecError::Runtime(<message>)` returns `(Error <original-atom> <message>)`
  atom;
- `ExecError::NoReduce` returns `NotReducible` atom;
- currently empty result removes result from the result set. It is done mainly
  for compatibility. There is no valid reason to return an empty result from a
  grounded function. Function can return `()/Empty/NotReducible` to express "no
  result"/"remove my result"/"not defined on data".

## chain

Minimal MeTTa implements normal evaluation order (see [Evaluation
order](#evaluation-order). Arguments are passed to the function without
evaluation. In case when argument should be evaluated before calling a function
one can use `chain` instruction.

`chain`'s signature is `(chain <atom> <var> <template>)` and it is executed in
two steps. `<atom>` argument is evaluated first and bindings of the evaluation
result are merged to the bindings of the current result. After that `chain`
substitutes all occurrences of `<var>` in `<template>` by the result of the
evaluation and returns result of the substitution. When evaluation of the
`<atom>` brings more than a single result `chain` returns one instance of the
`<template>` expression for each result.

## function/return

`function` operation has the signature `(function <atom>)`. It evaluates the
`<atom>` until it becomes `(return <atom>)`. Then `(function (return <atom>))`
expression returns the `<atom>`.

These operations are introduced for two reasons. First it should be possible to
evaluate an atom until some result and prevent further result evaluation. This
aspect is discussed in [eval or return](#eval-or-return) section.

Second without having an abstraction of a function call it is difficult to
debug the evaluation process. `function/return` allows representing nested
function calls as a stack and provide controls to put the breakpoints on parts
of this stack. Nevertheless using `chain` instead of `function` to implement
the evaluation loop also allows representing stack in a natural form.

## unify

`unify` operation allows conditioning on the results of the evaluation.
`unify`'s signature is `(unify <atom> <pattern> <then> <else>)`. The operation
matches `<atom>` with a `<pattern>`. If match is successful then it returns
`<then>` atom and merges bindings of the original `<atom>` to resulting
variable bindings. If matching is not successful then it returns the `<else>`
branch with the original variable bindings.

## cons-atom/decons-atom

`cons-atom` and `decons-atom` allows constructing and deconstructing the expression atom
from/to pair of the head and tail. `(decons-atom <expr>)` expects non-empty
expression as an argument and returns a pair `(<head> <tail>)`. `(cons-atom <head>
<tail>)` returns an expression where the first sub-atom is `<head>` and others
are copied from `<tail>`.

## collapse-bind

`collapse-bind` has the signature `(collapse-bind <atom>)`. It evaluates the
`<atom>` and returns an expression which contains all alternative evaluations
in a form `(<atom> <bindings>)`. `<bindings>` are represented in a form of a
grounded atom.

`collapse-bind` is part of the inference control provided by a minimal MeTTa
interpreter. For example it can be used to get all alternative interpretations
of the atom and filter out ones which led to errors.

Name `collapse-bind` is temporary and chosen to eliminate conflict with
`collapse` which is part of the standard library.

## superpose-bind

`superpose-bind` has the signature `(superpose-bind ((<atom> <bindings>)
...))`. It puts list of the results into the interpreter plan each pair as a
separate alternative.

`superpose-bind` is an operation which is complement to the `collapse-bind`.
`superpose-bind` takes the result of the `collapse-bind` as an input. Thus user
can collect the list of alternatives using `collapse-bind` filter them and
return filtered items to the plan using `superpose-bind`.

## Scope of a variable

Each separately evaluated expression is a variable scope, and therefore variable names are treated as unique inside an expression.
reason is that the whole expression is a variable scope. For example one can
write the expression `(chain (unify $parent Bob () ()) $_ $parent)`. And value
of the `$parent` is returned correctly.

When a variable is passed as an argument to a function call and matched by a
value then the value is assigned to the variable. If variable passed as an
actual argument is matched by a formal argument variable then it is referenced
by the formal argument variable. In this case the actual argument variable can
receive a value outsides of its scope.

For example the following code (written using MeTTa runner syntax) returns `B`:
```
(= (foo $b) (function (chain (unify B $b () ()) $_ (return ()))))
!(chain (eval (foo $a)) $_ $a)
```

If two separate expressions in the space have a variable with the same
name, but the variables reside in independent scopes, then the variables are different. Consider the
following example:
```
(= (foo) (function (chain (unify A $a () ()) $_ (return ()))))
!(chain (eval (foo)) $_ $a)
```
Here the value will not be assigned to the `$a` from the caller expression because
each of the two variables has a different scope and they do not reference each other.

# Examples

Examples of the programs written using minimal MeTTa interpreter:

Recursive switch implementation:

```metta
(= (switch $atom $cases)
  (function
    (chain (decons-atom $cases) $list
      (chain (eval (switch-internal $atom $list)) $res
        (unify $res NotReducible (return Empty) (return $res)) ))))

(= (switch-internal $atom (($pattern $template) $tail))
  (function
    (unify $atom $pattern
      (return $template)
      (chain (eval (switch $atom $tail)) $ret (return $ret)) )))
```

Evaluate atom in a loop until result is calculated:

```metta
(= (subst $atom $var $templ)
  (unify $atom $var $templ
    (Error (subst $atom $var $templ)
      \"subst expects a variable as a second argument\") ))

(= (reduce $atom $var $templ)
  (chain (eval $atom) $res
    (unify $res Empty
    Empty
    (unify $res (Error $a $m)
      (Error $a $m)
      (unify $res NotReducible
        (eval (subst $atom $var $templ))
        (eval (reduce $res $var $templ)) )))))
```

# Properties

## Turing completeness

The following program implements a Turing machine using the minimal MeTTa
instruction set (the full code of the example can be found
[here](../lib/src/metta/interpreter2.rs#L952-L995)):

```metta
(= (tm $rule $state $tape)
  (function (eval (tm-body $rule $state $tape))) )

(= (tm-body $rule $state $tape)
  (unify $state HALT
    (return $tape)
    (chain (eval (read $tape)) $char
      (chain (eval ($rule $state $char)) $res
        (unify $res ($next-state $next-char $dir)
          (chain (eval (move $tape $next-char $dir)) $next-tape
            (eval (tm-body $rule $next-state $next-tape)) )
          (return (Error (tm-body $rule $state $tape) \"Incorrect state\")) )))))

(= (read ($head $hole $tail)) $hole)

(= (move ($head $hole $tail) $char N) ($head $char $tail))
(= (move ($head $hole $tail) $char L) (function
  (chain (cons-atom $char $head) $next-head
    (chain (decons-atom $tail) $list
      (unify $list ($next-hole $next-tail)
        (return ($next-head $next-hole $next-tail))
        (return ($next-head 0 ())) )))))
(= (move ($head $hole $tail) $char R) (function
  (chain (cons-atom $char $tail) $next-tail
    (chain (decons-atom $head) $list
      (unify $list ($next-hole $next-head)
        (return ($next-head $next-hole $next-tail))
        (return (() 0 $next-tail)) )))))
```

## Comparison with MeTTa Operational Semantics

One difference from MOPS [1] is that the minimal instruction set allows
relatively easy write deterministic programs and non-determinism is injected
only via matching and evaluation. `Query` and `Chain` from MOPS are very
similar to `eval`. `Transform` is very similar to `unify`. `chain` has no
analogue in MOPS. `cons-atom`/`decons-atom` to some extent are analogues of
`AtomAdd`/`AtomRemove` in a sense that they can be used to change the state.

## Partial and complete functions

Each instruction in a minimal instruction set is a total function.
Nevertheless `Empty` allows defining partial functions in MeTTa. For example
partial `if` can be defined as follows:
```metta
(= (if $condition $then) (unify $condition True $then Empty))
```

## eval or return

Using `eval` to designate evaluation of the atom seems too verbose. But we need
to give a programmer some way to designate whether the atom should be evaluated
or not. `eval` marks atoms which should be evaluated. As an alternative to this
solution we could mark atoms which should not be evaluated.

Another related issue is that we need ability to make complex evaluations
before making a substitution inside `chain`. For example `(chain (eval (foo a))
$x $x)` should be able to make and fully evaluate the call of the `foo`
function before inserting the result into the template. We need to define the
criteria which specifies when the nested operation is finished and what is the
result. Also we need to be able represent evaluation loop inside the code.

First version of the minimal interpreter continued the evaluation of the first
argument of the `chain` until it becomes a non-minimal MeTTa instruction. But
this approach is too verbose. If it is needed to chain some minimal MeTTa
instruction without evaluation then such instruction should be wrapped into a
non-minimal MeTTa expression and unwrapped after the substitution is made.

```metta
  (chain (quote (eval (foo))) $x
    (unify $x (quote $y)
      $y
      (Error $x "quote expression expected") ))
```

To allow `chain` relying on the returned result of the first argument the
`function/return` operations are introduced. When user needs to run a complex
evaluation inside chain he may wrap it into the `function` operation.
`function` evaluates its argument in a loop until `(return <atom>)` is
returned. Then it returns the `<atom>` as a result. If one need to make a
substitution it is possible using:

```metta
  (chain (function (return <atom>)) <var> <templ>)
```

One more option is to make `chain` (and other atoms which can have nested
evaluation loops) recognize `return`. In such case the evaluation loop is
executed by the `chain` itself and `function` instruction is not needed.
Substitution gets the simpler form:

```metta
  (chain (return <atom>) <var> <templ>)
```

The downside of this approach is that loop represented by the outer operation
`chain` and end of the loop represented by `return` are written in different
contexts. Thus programmer should keep in mind that when some function is used
from `chain` and it is not just a equality substitution then `return` should be
used on each exit path while nothing in code of function points to this. Using
`function` operation allows dividing functions on two classes:
- functions which evaluate result in a loop and have to use `return`;
- functions which just replace the calling expression by their bodies.

# MeTTa interpreter written in Rust

MeTTa interpreter written in minimal MeTTa has poor performance. To fix this
the interpreter is rewritten in Rust. Rust implementation can be called using
`(metta <atom> <type> <space>)` operation. To be able represent process of the
interpretation as a list of steps and keep ability to control the inference
`metta` doesn't evaluate passed atom till the end but instead analyses the atom
and returns the plan written in minimal MeTTa. Plan includes steps written as a
Rust functions. These steps are called using `(call_native <name> <function>
<args>)` operation.

Both `metta` and `call_native` could be written as a grounded operations and be
a part of a standard library. But this requires grounded operations to be able
returning bindings as a results. Returning bindings as results is a nice to
have feature anyway to be able representing any functionality as a grounded
atom. But it is not implemented yet.

# Future work

## Explicit atomspace variable bindings

Current implementation implicitly keeps and applies variable bindings during
the process of the interpretation. Explicit bindings are used to implement
`collapse-bind` where they are absolutely necessary. Bindings can be easily
made explicit everywhere but the value of explicit bindings is not obvious see
[discussion in issue
#290](https://github.com/trueagi-io/hyperon-experimental/issues/290#issuecomment-1541314289).

Making atomspace part of the explicit context could make import semantics more
straightforward. In the current implementation of the minimal instruction set
it is needed to explicitly pass the atomspace to the interpreter because
otherwise grounded `get-type` function didn't work properly. It also could
allow defining `eval` via `unify` which minimizes the number of instructions
and allows defining `eval` in a MeTTa program itself. Which in turn allows
defining different versions of `eval` to program different kinds of chaining.
Nevertheless defining `eval` through `unify` requires rework of the grounded
functions interface to allow calling them by executing `unify` instructions.
Which is an interesting direction to follow.

## Special matching syntax

Sometimes it is convenient to change the semantics of the matching within a
pattern. Some real examples are provided below. One possible way to extend
matching syntax is embrace atoms by expressions with matching modifier on a
first position. For instance `(:<mod> <atom>)` could apply `<mod>` rule to
match the `<atom>`. How to eliminate interference of this syntax with symbol
atoms used by programmers is an open question.

### Syntax to match atom by equality

In many situations we need to check that atom is equal to some symbol. `unify`
doesn't work well in such cases because when checked atom is a variable it is
matched with anything (for instance `(unify $x Empty then else)` returns
`then`). It would be convenient to have a special syntax to match the atom by
equality. For instance `(unify <atom> (:= Empty) then else)` should match
`<atom>` with pattern only when `<atom>` is `Empty`.

### Syntax to match part of the expression

We could have a specific syntax which would allow matching part of the
expressions. For example such syntax could be used to match head and tail of
the expression without using `cons-atom`/`decons-atom`. Another example is matching part
of the expression with some gap, i.e. `(A ... D ...)` could match `(A B C D E)`
atom.

# Links

1. Lucius Gregory Meredith, Ben Goertzel, Jonathan Warrell, and Adam
   Vandervorst. Meta-MeTTa: an operational semantics for MeTTa.
   [https://raw.githubusercontent.com/leithaus/rho4u/main/ai/mops/mops.pdf](https://raw.githubusercontent.com/leithaus/rho4u/main/ai/mops/mops.pdf)
