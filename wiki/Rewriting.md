# Overview of Kiama's support for strategy-based rewriting

Up: UserManual, Prev: [Parsing](Parsing.md), Next: [Attribution](Attribution.md)

This page provides an overview of Kiama's support for _strategy-based
rewriting_. For the context in which this part of the library
operates, see [Context](Context.md). Rewriting is used in the following examples.

  * [Lambda](Lambda.md)
  * [Lambda2](Lambda2.md)
  * [Oberon0](Oberon0.md)
  * [TIL](TIL.md)

More information about Kiama rewriting can be found via the [Research](Research.md)
page.

## Introduction

Many language processing tasks are best formulated as _program
transformation problems_. Examples include desugaring of high-level
language constructs into combinations of more basic ones, evaluation
of languages such as lambda calculus by repeated application of
reduction rules, optimisation of languages by application of
transformations designed to improve program efficiency and translation
from a source language to a target language.

Kiama supports program transformation via a form of strategy-based
term rewriting, heavily based on the [Stratego](http://strategoxt.org/)
language and library. Most transformations that can be expressed by
Stratego can written in a similar fashion in Kiama and hence much
of the Stratego documentation and literature also applies to Kiama.
Exceptions will be noted in the following.

The Kiama rewriting library is in the object `org.kiama.rewriting.Rewriter`.
Import the types and methods you need from that object.  (There is
also an `Rewriter` trait so you can mix it in with your own
functionality.)

For example,

```
import org.kiama.rewriting.Rewriter._
```

imports all names from that object.

The rewriting library uses `Term` to refer to the type of data that
can be rewritten. This type is defined to be `Any`. Usually custom
structures are implemented with case classes which are automatically
supported by Kiama's generic traversals, as are traversable
collections. See [Context](Context.md) for more information.

## Strategies

The basic unit of computation in strategy-based rewriting is the
_strategy_ which are represented by values of the `Strategy` type in
Kiama. When applied to a particular term, a strategy either succeeds,
producing a rewritten term, or fails. The term that a strategy is
operating on is the _subject term_.

A strategy in Kiama is a function from the subject term to an `Option`
value, where `Some (t)` represents a successful rewrite to a new
subject term `t` and `None` represents failure.

```
abstract class Strategy extends (Term => Option[Term]) { ... }
```

Since `Strategy` is abstract, strategies must be created using the
combinators described in the following.

A strategy `s` can be applied to a particular subject term `t` by
simply applying it as a normal Scala function, i.e., using `s (t)`.
The resulting `Option` value can be pattern-matched as usual.

`rewrite` provides a more convenient way to apply strategies.

```
def rewrite (s : => Strategy) (t : Term) : Term
```

`rewrite (s) (t)` applies the strategy `s` to the term `t`. If `s`
succeeds, `rewrite` returns the term that results; if `s` fails,
rewrite returns `t`.

## Basic strategies

The most basic strategies in Kiama are `id`, which always succeeds and
returns the subject term unchanged, and `fail`, which always fails.

Another kind of basic strategy is constructed by the `term` method:

```
def term (t : Term) : Strategy
```

`term (t)` succeeds if and only if the subject term is the term t.

It is sometime useful to set the subject term to be a specific term.
This can be done in Kiama using `build`.

```
def build (t : => Term) : Strategy
```

`build (t)` returns a strategy that always succeeds with result `t`.

Sometimes you already have an `Option[Term]` value that you want
to inject into the strategy world.  For example, you might have
a method that searches a data structure for something, returning
an `Option` to indicate whether it was there or not.
`option` makes a strategy that succeeds or fails depending on its
argument option value.  If the option is a `Some` then the strategy
will succeed with the wrapped value, otherwise it will fail.

```
def option (o : Option[Term]) : Strategy
```

## Strategies from functions

One way to create non-trivial strategies in Kiama is by starting with
a regular Scala function and using the `strategy` and `strategyf`
combinators to lift them to `Strategy`.

```
def strategy (f : Term ==> Option[Term]) : Strategy
def strategyf (f : Term => Option[Term]) : Strategy
```

(`T ==> U` is a Kiama-wide alias for `PartialFunction[T,U]`.)

If you already have a (possibly partial) function from `Term` to
`Option[Term]` then you can use `strategy` or `strategyf` to make it
into a strategy. The strategy resulting from an application of
`strategy` will succeed if and only if the argument function `f`
produces `Some (t)` for some result term `t` when applied to the
subject term. If `f` is not defined or produces `None`, then the
strategy will fail. Similarly, the strategy resulting from an
application of `strategyf` will succeed or fail entirely based on the
return value of `f`.

As an example of the use of `strategyf`, the `id` and `fail`
combinators can be defined as follows.

```
val fail = strategyf (_ => None)
val id = strategyf (t => Some (t))
```

Another example that is defined using `strategyf` is `debug (s)` which
is the same as `id`, except that it prints the string `s` followed by
the subject term as a side-effect. (Kiama also has `log` and `logfail`
combinators that are useful for printing messages as rewriting
proceeds.)

A common use case for `strategy` is when a strategy is defined
directly in terms of another strategy (or combination of strategies).
For example, given `issubterm` that matches on a pair of terms `(x,y)`
and succeeds if `x` is a sub-term of `y`, we can define `issuperterm`
as follows.

```
val issuperterm : Strategy =
    strategy {
        case (x, y) => issubterm (y, x)
    }
```

## Rewrite rules

A _rewrite rule_ is a strategy that performs pattern matching on the
subject term and, depending on the result of the match, returns a new
subject term. In Kiama, rewrite rules are built using the `rule`,
`rulef` and `rulefs` combinators.

```
def rule (f : Term ==> Term) : Strategy
def rulef (f : Term => Term) : Strategy
def rulefs (f : Term ==> Strategy) : Strategy
```

`rule` and `rulef` operate similarly to `strategy` and `strategyf`,
except that the argument function `f` must return a `Term` instead of
an `Option[Term]`. Thus, a strategy constructed using `rulef` cannot
fail and one constructed using `rule` only fails if the `f` is not
defined at the subject term.

In Scala it is possible to create anonymous partial functions in a
convenient fashion using pattern matching expressions. Here is an
example of the most common usage style for `rule`.

```
rule {
   case Add (Num (i), Num (j)) => Num (i + j)
   case Sub (Num (i), Num (j)) => Num (i - j)
}
```

Patterns are used in this rule to deconstruct the subject term, check
for a particular structure and bind the variables `i` and `j` to
components of that structure. The bound variables are used to help
construct the result terms. If the subject term does not match either
of the given patterns, then the rule fails.

`rulefs` is slightly different to `rule` and `rulef` in that its argument
returns a strategy, not a term.  The returned strategy is applied to the
subject term.  Therefore, rulefs` is most useful in situations where you
want to match some pattern to bind some variables, then use those
variables in another strategy.  See the
File: [ParLazy.scala](https://code.google.com/p/kiama/source/browse/library/src/org/kiama/example/lambda2/ParLazy.scala)
of the lambda2 example for uses of `rulefs`.

Stratego allows the use of concrete syntax in strategies. For example
`Add (Num (i), Num (j))` might be written `|[ i + j ]|` with the aid
of a parser for the source language. Kiama does not currently support
concrete syntax.

## Queries

A variant of a rule is a _query_ that always succeeds, but is run for
its side-effects. For example, a query might be used to extract
information from a term without changing it.  In Kiama queries are
created using the `query` combinator.

```
def query[T] (f : Term ==> T) : Strategy
```

`query (f) (t)` always succeeds with result `t` regardless of the
result of `f (t)`.  For example, given

```
var vars = Set[String]()
```

a query

```
query { case Var (s) => vars += s }
```

might be used to collect variable accesses from within a term.
(See below for how to specify the term traversal.)

As for `strategy` and `rule`, there is `queryf` version of `query`
that takes a total function instead of a partial one.

## Combining local strategies

The combinators described so far suffice to define simple local
transformations, but more is needed to define realistic
transformations of whole terms. First, we describe how to combine
strategies that are applied to the same part of the term, then later,
how to traverse a term to apply strategies to different sub-terms.

The following methods are available in the `Strategy` class to combine
a strategy with another.

```
def <* (q : => Strategy) : Strategy
def <+ (q : => Strategy) : Strategy
def + (q : => Strategy) : PlusStrategy
```

The simplest combination of two strategies `p` and `q` is the sequence
of `p` then `q`, written `p <* q`. (Stratego uses a semicolon operator
for sequencing, but this is unavailable to us in Scala.) `p <* q`
first applies `p` to the subject term. If it succeeds, `q` is then
applied to the result of `p`. The success or failure of `q` is then
the outcome for the whole sequence. If `p` fails, then the sequence
fails without `q` being applied.

`p <+ q` expresses a deterministic choice between `p` and `q`. If `p`
succeeds when applied to the subject term, then its result is the
result of the choice. If `p` fails, then `q` is applied to the subject
term and its success or failure is the outcome for the choice.

`p + q` is non-deterministic choice, where either `p` or `q` may be
applied first.  In Kiama, non-deterministic choice is currently
implemented as deterministic choice.

Three strategies `p`, `q` and `r` can be combined in a conditional
choice expression `p < q + r` using the `<` combinator:

```
def < (q : => PlusStrategy) : Strategy
```

`p < q + r` first applies `p` to the subject term. If it succeeds,
then `q` is applied to the result of `p` to determine the overall
outcome. If `p` fails, then `r` is applied to the original subject
term to determine the outcome.

See the discussion of library strategies below for examples of the
use of the sequence and choice combinators.

## Bound variable scope

Because Kiama uses regular Scala pattern matching, the scope of bound
variables in patterns is just the right-hand side of the matching
case. This situation contrasts with Stratego where pattern matching is
a fundamental construct that enables bindings to be propagated through
applications of the basic combinators. For example, in Stratego it is
possible to bind a variable in one component of a sequence and use the
variable in a later component of the sequence. This cannot be done in
the same way in Kiama since the bindings occur within rules, not at
the strategy level. Instead it would be necessary to invoke the second
component of the sequence in the right-hand side of the case that
binds the relevant variables.

## Traversals

The combinators described so far do not enable traversal into a
subject term. There are a number of ways to achieve a traversal.

In Kiama, as in Stratego, one way to perform traversal is to
explicitly encode it. For example, assuming we have

```
def eval (exp : Exp) : Exp
```

we can explicitly apply `eval` to components of a term, thereby
encoding a traversal into the term.  For example,

```
rule {
    case App (e1, e2)       => App (eval (e1), eval (e2))
    case Let (x, t, e1, e2) => Let (x, t, eval (e1), eval (e2))
    case Opn (op, e1, e2)   => Opn (op, eval (e1), eval (e2))
}
```

A more generic and composable approach to traversal is provided
by the `all`, `one` and `some` combinators that Kiama inherits
from Stratego.

```
def all (s : => Strategy) : Strategy
def one (s : => Strategy) : Strategy
def some (s : => Strategy) : Strategy
```

`all` constructs a strategy that applies the strategy `s` to all of
the children of the subject term `t` in left-to-right order. If `s`
succeeds on all of the children, a new subject term is created using
the constructor of `t` and the rewritten children. If `s` fails on any
child of `t`, then the application of `all (s)` fails.

`one` and `some` are similar to `all`. `one` applies `s` to the
children as for `all` except that it succeeds as soon as application
of `s` to a child `c` succeeds. `some` provides a middle ground
between `all` and `one` as it builds a result that contains rewritten
children for just the children on which `s` succeeds.

Examples of the use of the generic traversal combinators are given
later on this page.

## Congruences

In Stratego it is common to use _congruences_ to define traversals.
For example, the congruence `App (eval, eval)` would specify the same
traversal as the `case App (e1, e2) => App (eval (e1), eval (e2))`
case in the above example. A _congruence_ in Stratego terminology is a
strategy that is defined to apply other strategies to selected parts
of a particular structure. For example, the congruence `App (s1, s2)`
applies the `s1` strategy to the first component of an `App` term and
the `s2` strategy to the second component. The congruence fails if the
term is not an App, or if either of `s1` and `s2` fails.

Congruences are very convenient for specifying fixed patterns of
traversal.  Kiama offers some support for them, but cannot provide
exactly the same semantics without some support from the developer.
To use a congruence on one of your data structures, you need to
provide a function like the one below for each constructor.

```
def App (s1 : => Strategy, s2 : => Strategy) : Strategy =
    rulefs {
        case _ : App =>
            congruence (s1, s2)
    }
```

Now, `App (s1, s2)` can be used to get the desired behaviour.
This function ensures two things: that the congruence is applied to
exactly two strategies (since `App` has two children) and that the
resulting strategy only works for `App` terms.  The actual work of
applying the strategies and constructing the new term is done by
the generic Kiama function `congruence`.  In this case we have
named the congruence `App` to emphasise the connection to the
`App` type, but in modules where the `App` constructor needs to
be used as well, a different name should be used for the congruence.

## Preservation of terms under absence of rewriting

The generic traversal combinators, congruences, and any other Kiama
combinator that provides generic access to a term's children, preserve
the subject term if no rewriting actually occurs. In other words, if a
strategy constructed by them succeeds but makes no change to the
children of the subject term, then the strategy succeeds with the
subject term itself. This property means a strategy that simply
traverses a structure to collect some information, but performs no
actual rewriting, will not build a new structure; it will just return
the old one.

## Strategy library

Kiama includes many useful combinators from the Stratego library. A
few are presented here to illustrate higher-order strategies and the
Kiama combinators. Consult the Kiama API documentation for a complete
list.

`attempt` constructs a strategy that applies `s`, restoring the
original term if `s` fails.  (In Stratego `attempt` is called `try`
which is unavailable to Kiama since it is a Scala keyword.)

```
def attempt (s : => Strategy) : Strategy =
    s <+ id
```

`repeat` repeatedly applies s to the subject term until it fails,
returning the term resulting from the last successful application.

```
def repeat (s : => Strategy) : Strategy =
    attempt (s <* repeat (s))
```

`reduce` is similar to `repeat` except it also tries to apply `s`
to a sub-term.

```
def reduce (s : => Strategy) : Strategy = {
    def x : Strategy = some (x) + s
    repeat (x)
}
```

`topdown` constructs a strategy that applies `s` in a top-down, prefix
fashion. `s` is first applied to the subject term. If that succeeds,
`all` is used to recursively apply `topdown (s)` to the children of
the result. The traversal into each branch of the tree stops whenever
`s` fails.

```
def topdown (s : => Strategy) : Strategy =
    s <* all (topdown (s))
```

In contrast, `bottomup` applies `s` in the opposite direction.

```
def bottomup (s : => Strategy) : Strategy =
    all (bottomup (s)) <* s
```

A common idiom is to combine rewrite rules that apply to different
kinds of terms using a choice combinator. For example, given rules for
beta reduction and arithmetic operation evaluation in lambda calculus

```
val beta =
    rule {
        case App (Lam (x, _, e1), e2) => substitute (x, e2, e1)
    }

val arithop =
    rule {
        case Opn (op, Num (l), Num (r)) => Num (op.eval (l, r))
    }
```

we can combine them into an evaluator that repeatedly reduces expressions
as follows.

```
reduce (beta + arithop)
```

Finally, as noted earlier, Kiama deals with variable bindings in
patterns in a different way to Stratego since Kiama uses standard
Scala patterns. As a result it is sometime useful to invoke a strategy
in the definition of another strategy after performing pattern
matching.  For example, a strategy `issubterm` might be defined in
Stratego as follows.

```
issubterm =
    ?(x, y); where(<oncetd(?x)> y)
```

In this definition the pattern match `?(x,y)` is binding `x` and `y`
for use in the second part of the sequence. In Kiama `issubterm` can
be written as follows.

```
val issubterm : Strategy =
    strategy {
        case (x : Term, y : Term) => where (oncetd (term (x))) (y)
    }
```

## Cloning

Usually the structure that is being rewritten is a tree. Therefore,
usually care must be taken to ensure that the rewritten structure
is also a tree. In particular, if you rewrite a term so that it
appears more than once in the new term, you will probably want to
clone it before inserting it into the new term.

Cloning is particularly important if you are going to perform
[attribution](Attribution.md) on the new structure, since attributes
are associated with nodes. If a node appears more than once in
the structure it will have the same attributes at each place,
which is probably not what you want. Kiama's
[Attributable](Attribution#Attributable.md) trait provides
`clone` and `deepclone` operations that can be used to avoid
this problem.

In some cases it is ok to produce structures that are graphs,
particularly acyclic ones. In those cases, you should be able to
do without cloning copied nodes.

## Types

As in Stratego, Kiama currently treats terms as generic values, so,
for example, there is no difference in type between a strategy that
transforms an `Exp` into an `Exp` and one that transforms an `Exp`
into something else. We are investigating ways to use Scala's type system to
provide more static safety.

Although terms are generic values from the strategy perspective, of
course construction of specific terms using case class constructors is
statically checked by Scala's type system. This eliminates one
potential source of errors when writing rewrite rules, in particular.

## Callbacks

Sometimes it is useful to be able to intercede in the rewriting process
in a way that is specific to the problem that is being solved. For example,
when are rewriting terms that contain source coordinate positions it is
often important to copy positions into the new terms. The normal Kiama
rewriter doesn't do this, but there is a callback mechanism that can be
used to achieve this effect.

The `CallbackRewriter` extension of `Rewriter` performs the same rewrites
as `Rewriter`, but each rewriting operation will also call a method
`rewriting` that you can define.

```
def rewriting[T <: Term] (oldTerm : T, newTerm : T) : T
```

`rewriting` will be passed the term that is being rewritten (`oldTerm`)
and the term it is being rewritten into (`newTerm`). It should return
a term of the same type, which would usually be `newTerm`, but doesn't
have to be.

`rewriting` will be called for each rewriting operation performed by
strategies that are constructed by `rule` and related methods as well
as the product duplication operation performed during generic traversals.
Note that in the case of `rule` and similar, `rewriting` will only be
called on the root of a term that matches the left-hand side of a
rule and the root of the term that it is being rewritten into. If you
want to affect terms inside the new term, for example, you will need
to do that in `rewriting`.

The `PositionalRewriter` extension of `CallbackRewriter` defines the
`rewriting` method to copy positions from the old terms to the new
terms. Terms that implement `scala.util.parsing.input.Positional` are
recognised as having positions by this operation.

Up: UserManual, Prev: [Parsing](Parsing.md), Next: [Attribution](Attribution.md)
