This document describes the minimal set of embedded MeTTa instructions which is
enough to write the complete MeTTa interpreter in MeTTa. Current version of the
document includes improvements which were added after experimenting with the
first version of such an interpreter. It is not a final version and some
directions of the future work is explained at the end of the document.

# Minimal instruction set

## Interpreter state

The MeTTa interpreter interprets an atom passed as an input. It interprets it
step by step on each step executing the single instruction. In order to do that
the interpreter needs a context which is wider than the atom itself. The
context also includes (see Explicit atomspace variable bindings):
- an atomspace which contains the knowledge which drives the evaluation of the
  expressions;
- bindings of the variables which are used to evaluate expressions; the
  bindings are empty at the beginning.

Each step of interpretation inputs and outputs a list of pairs (`<atom>`,
`<bindings>`) which is called an interpretation plan. Each pair in the plan
represents one possible way of interpreting the original atom or possible
branch of the evaluation. Interpreter doesn't select one of them for further
processing. It continues interpreting all of the branches in parallel. This is
what is called non-deterministic evaluation.

One step of the interpretation is an execution of a single instruction from a
plan. An interpreter removes the atom from the plan, executes it and adds the
results back to the plan. Here we suppose that on the top level the plan
contains only the instructions from the minimal set. It is not necessary but it
allowed developing the first stable version with less effort (see `eval` and
`Return`). If an instruction returns the atom which is not from the minimal set
it is not interpreted further and returned as a part of the final result.

## Error/Empty/NotReducible/Void

There are atoms which can be returned to designate a special situation in a code:
- `(Error <atom> <message>)` means the interpretation is finished with error;
- `Empty` means the corresponding branch of the evaluation returned no results,
  such result is not returned among other results when interpreting is
  finished;
- `NotReducible` can be returned by `eval` in order to designate the situation
  when function can not be reduced further; for example it can happen when code
  tries to call a type constructor (which has no definition), partially defined
  function (with argument values which are not handled), or grounded function
  which returns `NotReducible` explicitly; this atom is introduced to separate
  the situations when atom should be returned "as is" from `Empty` when atom
  should be removed from results;
- `Void` is a unit result which is mainly used by functions with side effects
  which has no meaningful value to return.

These atoms are not interpreted further as they are not a part of the minimal
set of instructions.

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
returns no results then `NotReducible` atom is a result of the instruction. Grounded
function can return a list of atoms, empty result, `Error(<message>)` or
`NoReduce` result. The result of the instruction for a special values are the
following:
- empty result returns `Void` atom;
- `Error(<message>)` returns `(Error <original-atom> <message>)` atom;
- `NoReduce` returns `NotReducible` atom.

## chain

A function call with a sub-expression as an argument can be interpreted using
two different orders. The interpreter can evaluate the whole expression or
evaluate the argument first and then insert the result into the original
expression. The proper order of interpretation can depend on the type of the
function.

`chain` instruction allows a programmer to control the order of the evaluation
and interpret the subexpression first. It has a form `(chain <atom> <var>
<template>)` and executed differently depending on what kind of `<atom>` is passed
as a first argument:
- if `<atom>` is an instruction from the minimal set then chain executes it and
  returns `(chain <execution-result> <var> <template>)`; bindings of the <atom>
  are merged to the bindings of the result;
- it substitutes all occurrences of `<var>` in `<template>` by the `<atom>`
  otherwise.  When execution of the `<atom>` brings more than a single result
  `chain` returns one `(chain ...)` expression for each result. `chain` can be
  nested in such a case only the most nested `chain` instruction is executed
  during the interpretation step.

## unify

Conditioning on the results can be done using `unify` operation `(unify <atom>
<pattern> <then> <else>)`. This operation matches `<atom>` with a `<pattern>`. If
match is successful then it returns `<then>` atom and merges bindings of the
original `<atom>` to resulting variable bindings. If matching is not successful
then it returns the `<else>` branch with the original variable bindings.

## cons/decons

Last pair of operations are `cons` and `decons` to construct and deconstruct
the expression atom from/to head and tail. `(decons <expr>)` returns:
- `()` when `<expr>` is empty;
- `(<head> <tail>)` when `<expr>` is not empty.

`(cons <head> <tail>)` returns an expression where the first sub-atom is
`<head>` and others are copied from `<tail>`.

# Examples

Examples of the programs written using minimal MeTTa interpreter:

Switch implementation:

```metta
(= (switch $atom $cases)
  (chain (decons $cases) $list
    (chain (eval (switch-internal $atom $list)) $res
      (unify $res NotReducible Empty $res) )))
(= (switch-internal $atom (($pattern $template) $tail))
  (unify $atom $pattern $template (eval (switch $atom $tail))))
```

Reduce in loop until result is calculated:

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

[Link](https://github.com/trueagi-io/hyperon-experimental/blob/27861e63af1417df4780d9314eaf2e8a3b5cde06/lib/src/metta/runner/stdlib2.rs#L234-L302)
to the full code of the interpreter in MeTTa (not finished yet).

# Properties

## Turing completeness

The following program implements a Turing machine using the minimal MeTTa
instruction set (the full code of the example can be found
[here](https://github.com/trueagi-io/hyperon-experimental/blob/27861e63af1417df4780d9314eaf2e8a3b5cde06/lib/src/metta/interpreter2.rs#L628-L669)):

```metta
           (= (tm $rule $state $tape)
              (unify $state HALT
                $tape
                (chain (eval (read $tape)) $char
                  (chain (eval ($rule $state $char)) $res
                    (unify $res ($next-state $next-char $dir)
                      (chain (eval (move $tape $next-char $dir)) $next-tape
                        (eval (tm $rule $next-state $next-tape)) )
                      (Error (tm $rule $state $tape) \"Incorrect state\") )))))

            (= (read ($head $hole $tail)) $hole)

            (= (move ($head $hole $tail) $char N) ($head $char $tail))
            (= (move ($head $hole $tail) $char L)
              (chain (cons $char $head) $next-head
                (chain (decons $tail) $list
                  (unify $list ($next-hole $next-tail)
                    ($next-head $next-hole $next-tail)
                    ($next-head 0 ()) ))))
            (= (move ($head $hole $tail) $char R)
              (chain (cons $char $tail) $next-tail
                (chain (decons $head) $list
                  (unify $list ($next-hole $next-head)
                    ($next-head $next-hole $next-tail)
                    (() 0 $next-tail) ))))
```

## Comparison with MeTTa Operational Semantics

One difference from MOPS [1] is that the minimal instruction set allows
relatively easy write deterministic programs and non-determinism is injected
only via matching and evaluation. `Query` and `Chain` from MOPS are very
similar to `eval`. `Transform` is very similar to `unify`. `chain` has no
analogue in MOPS, it is used to make deterministic computations.
`cons`/`decons` to some extent are analogues of `AtomAdd`/`AtomRemove` in a
sense that they can be used to change the state.

## eval or return

Using `eval` to designate evaluation of the atom seems too verbose. But we need
to give a programmer some way to designate whether the atom should be evaluated
or not. `eval` marks atoms which should be evaluated. As an alternative to this
solution we could mark atoms which should not be evaluated.

For example we could use a special instruction `(return <atom>)` which
basically does nothing. When `return` is on the top level of the interpretation
plan then the interpreter puts `<atom>` into the list of the final results. Other
atoms on the top of the plan are evaluated. `chain` should be changed to have
the same semantics: evaluate any atom except `(return ...)` and insert the `(return
...)` into the template.

The version of the `reduce` written using `return` will look like the following:
```metta
(= (reduce $atom $var $templ)
  (chain $atom $res
    (unify $res (return (Error $a $m))
      (return (Error $a $m))
      (unify $res (return Empty)
        (subst $atom $var $templ)
        (unify $res (return $val)
          (subst $val $var $templ)
          (reduce $res $var $templ) )))))
```

This version has one more interesting property: a programmer can use `(return
<atom>)` to designate that the process of reducing should be finished. Otherwise
the reduction stops only when an atom cannot be evaluated further. Thus the
`return` idea can also be used in the MeTTa interpreter to control an execution
and improve performance.

We could also implement `chain` which removes `return` and inserts `<atom>`
into the template. It can make program even more compact:
```metta
(= (reduce $atom $var $templ)
  (chain $atom $res
    (unify $res (Error $a $m)
      (Error $a $m)
      (unify $res Empty
        (subst $atom $var $templ)
        (reduce $res $var $templ) ))))
```

But in such a case there is a risk of getting `(chain (return <atom>) $x $x)`
which continues evaluation of the `<atom>` further while it is not expected.

The final convention can be chosen from a usability perspective. Although it
may seem that eliminating `eval` is the most convenient solution, it is not
obvious. `eval` is a kind of `call` in assembly language and writing it
explicitly makes the author think about whether the atom can be called.

## Partial and complete functions

Each instruction in a minimal instruction set is a complete function.
Nevertheless `Empty` allows defining partial functions in MeTTa. For example
partial `if` can be defined as follows:
```metta
(= (if $condition $then) (unify $condition True $then Empty))
```

# Future work

## Explicit atomspace variable bindings

Current implementation implicitly keeps and applies variable bindings during
the process of the interpretation. It can be easily made explicit but the value
of explicit bindings is not obvious see [discussion in issue
#290](https://github.com/trueagi-io/hyperon-experimental/issues/290#issuecomment-1541314289).

Making atomspace out of implicit context could make import semantics more
straightforward. In the current implementation of the minimal instruction set
it was needed to explicitly pass the atomspace to the interpreter because
otherwise grounded `get-type` function didn't work properly.It also could allow
defining `eval` via `unify` which minimizes the number of instructions and
allows defining `eval` in a MeTTa program itself. Which in turn allows defining
different versions of `eval` to program different kinds of chaining.
Nevertheless defining `eval` through `unify` requires rework of the grounded
functions interface to allow calling them by executing `unify` instructions.
Which is an interesting direction to follow.

## Scope of variables

Scope of the variable inside instructions is not described in this
specification. It is a clear gap and one of the todo items.

## Collapse

The described language allows transforming the atom into a non-deterministic
result using `eval`, but there is no reversed instruction. As a consequence one
cannot implement `collapse` using the instruction set above. In order to add
such possibility the additional instruction is needed. It could mark a set of
results as joined and when their evaluation is finished would assemble them
into an expression.

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
the expression without using `cons`/`decons`. Another example is matching part
of the expression with some gap, i.e. `(A ... D ...)` could match `(A B C D E)`
atom.

# Links

1. Lucius Gregory Meredith, Ben Goertzel, Jonathan Warrell, and Adam
   Vandervorst. Meta-MeTTa: an operational semantics for MeTTa.
   [https://raw.githubusercontent.com/leithaus/rho4u/main/ai/mops/mops.pdf](https://raw.githubusercontent.com/leithaus/rho4u/main/ai/mops/mops.pdf)
