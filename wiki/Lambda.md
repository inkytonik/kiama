# Example: Simple rewriting-based evaluation of lambda calculus

Up: [Examples](Examples.md), Prev: [Imperative](Imperative.md), Next: [Lambda2](Lambda2.md)

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

This example shows how Kiama's rewriting library can be used to implement
a simple un-typed version of the
[lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus).

## Abstract syntax

File: [org.bitbucket.inkytonik.kiama.example.lambda.Lambda.scala](https://github.com/inkytonik/kiama/blob/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Lambda.scala)

Programs for this example consist of expressions.

```
abstract class Exp
```

Leaf expressions are integer numbers (`Num`) or variables (`Var`).

```
case class Num (i : Int) extends Exp
case class Var (x : Idn) extends Exp
type Idn = String
```

A lambda expression (`Lam`) creates an anonymous function with a
single formal argument and a body expression that may refer to that
argument. In this example, we assume that the bound variable in a
lambda expression is not bound elsewhere in the expression being
evaluated.

```
case class Lam (x : Idn, e : Exp) extends Exp
```

An application (`App`) applies a function to an actual argument
expression.

```
case class App (l : Exp, r : Exp) extends Exp
```

This version of lambda calculus has explicit substitutions (as opposed
to implementing them in the meta-language) represented by `Sub`
expressions. In an instance of `Sub`, we are substituting `n` for `x`
within `m`.

```
case class Sub (m : Exp, x : Idn, n : Exp) extends Exp
```

## Parser

File: [org.bitbucket.inkytonik.kiama.example.lambda.Lambda.scala](https://github.com/inkytonik/kiama/blob/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Lambda.scala)

The [parser](Parsing.md) is a simple application of Scala's [parser combinators](ParserCombs.md).
(See the [Imperative](Imperative.md) example for more explanation of a similar parser.)

```
lazy val start =
    phrase (exp)

lazy val exp : PackratParser[Exp] =
    exp ~ factor ^^ App |
    ("\\" ~> idn) ~ ("." ~> exp) ^^ Lam |
    factor |
    failure ("expression expected")

lazy val factor : PackratParser[Exp] =
    integer | variable | "(" ~> exp <~ ")"

lazy val integer =
    "[0-9]+".r ^^ (s => Num (s.toInt))

lazy val variable =
    idn ^^ Var

lazy val idn =
    "[a-zA-Z][a-zA-Z0-9]*".r
```

## Evaluation

File: [org.bitbucket.inkytonik.kiama.example.lambda.Lambda.scala](https://github.com/inkytonik/kiama/blob/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Lambda.scala)

We use an evaluation scheme from lecture notes by Kristoffer H. Rose,
[Explicit Substitution - Tutorial and Survey](http://www.brics.dk/LS/96/3/BRICS-LS-96-3/BRICS-LS-96-3.html).
Normal order reduction is defined as outermost application of the
`xgc_reduction` rule.

```
val normal = outermost (xgc_reduction)
```

`outermost` comes from the Kiama rewriting library and builds a
strategy that applies its argument once in a top-down fashion and then
repeats on the result. Rewriting stops when the argument rewrite rule
fails to apply.

`xgc_reduction` is defined by a single Kiama rewrite rule that pattern
matches on the term that is being reduced.

```
val xgc_reduction =
    rule {
        ... cases ...
    }
```

We consider each case in turn. The main one is a version of _beta
reduction_ that applies a function by substituting the actual argument
into the body in place of the formal argument. Since substitutions are
explicit, this is a simple term rewrite rather than an invocation of a
meta-level substitution function.

```
case App (Lam (x, e1), e2) => Sub (e1, x, e2)
```

The remaining cases all deal with substitution. If we are substituting
into a variable then something happens only if the variable we have is
the same as the variable for which we are substituting.

```
case Sub (Var (x), y, n) if (x == y) => n
case Sub (Var (x), _, _)             => Var (x)
```

If we are substituting in a lambda expression, we propagate the
substitution to the body of the lambda expression. Of course, this
only works in general if the bound variables are unique, as assumed by
Convention A.3.4 in the Rose notes.

```
case Sub (Lam (x, m), y, n) => Lam (x, Sub (m, y, n))
```

An alternative formulation relies on a function `FreshVar` that can
produce fresh variables names (i.e., ones that are guaranteed not to
occur elsewhere in the program). Using `FreshVar` we can formulate
the lambda expression substitution so that Convention A.3.4 does
not have to be assumed. In this version we rename the bound
variable `x` to a fresh name `xprime` before we perform the
substitution for `y` so that capturing of `x` cannot occur.

```
case Sub (Lam (x, m), y, n) =>
    val xprime = FreshVar ()
    Lam (xprime, Sub (Sub (m, x, Var (xprime)), y, n))
```

Substitution in an application is propagated to the two sides of the
application.

```
case Sub (App (m1, m2), y, n) => App (Sub (m1, y, n), Sub (m2, y, n))
```

Finally, if we are substituting for a variable that doesn't occur free
within the expression then we can drop it to implement a form of
garbage collection.

```
case Sub (m, x, n) if ! (fv (m) contains (x)) => m
```

## Free variables

File: [org.bitbucket.inkytonik.kiama.example.lambda.Lambda.scala](https://github.com/inkytonik/kiama/blob/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Lambda.scala)

The auxiliary function `fv` provides the set of variables that are
free (i.e., not bound) in a given expression.

```
def fv (t : Exp) : Set[Idn] = {
    t match {
        case Num (_)       => Set ()
        case Var (x)       => Set (x)
        case Lam (x, e)    => fv (e) - x
        case App (m, n)    => fv (m) ++ fv (n)
        case Sub (m, x, n) => (fv (m) - x) ++ fv (n)
    }
}
```

## Running

File: [org.bitbucket.inkytonik.kiama.example.lambda.Lambda.scala](https://github.com/inkytonik/kiama/blob/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Lambda.scala)

To run the read-eval-print loop to parse programs, evaluate them to a normal form
and print the result:

```
sbt 'main org.bitbucket.inkytonik.kiama.example.lambda.Lambda'
Enter lambda calculus expressions for evaluation.
lambda> (\x.99) 42
99
lambda> (\y.(\z.z) y) x
x
lambda>
```

To run a read-eval-print loop that generates random expressions:

```
sbt 'main org.bitbucket.inkytonik.kiama.example.lambda.LambdaGen'
Each time you hit ENTER a new instance is generated and printed.
Hit ENTER to generate an instance:
(\dw9je.(((cna60 48) uxdbp) ((\qyf1U.10) (\dplpg.(45 26)))))
Hit ENTER to generate an instance:
```

## Tests

File: [org.bitbucket.inkytonik.kiama.example.lambda.LambdaTests.scala](https://github.com/inkytonik/kiama/blob/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/LambdaTests.scala)

Some simple tests of lambda calculus evaluation.

```
sbt 'test-only org.bitbucket.inkytonik.kiama.example.lambda.LambdaTests'
```

Up: [Examples](Examples.md), Prev: [Imperative](Imperative.md), Next: [Lambda2](Lambda2.md)
