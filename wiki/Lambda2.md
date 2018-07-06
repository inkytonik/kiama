# Example: More advanced implementation of lambda calculus

Up: [Examples](Examples.md), Prev: [Lambda](Lambda.md), Next: [Oberon0](Oberon0.md)

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

An implementation of simply-typed
[lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus) with a
selection of evaluation mechanisms. Name and type analysis are
implemented using [Attribution](Attribution.md) and the evaluation mechanisms are
implemented using [Rewriting](Rewriting.md). A [read-eval-print loop](ReadEvalPrintLoops.md)
enables expressions to be parsed, semantically checked and then evaluated
with a user-selected mechanism.

## Abstract syntax

File: [org.bitbucket.inkytonik.kiama.example.lambda2.LambdaTree.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/LambdaTree.scala)

Programs for this example consist of expressions represented by the
`Exp` type. `Exp` extends [Attributable](Attribution.md#markdown-header-attributable)
so that later we can define attributes for expressions.

```
abstract class Exp extends Attributable with Positional
```

Leaf expressions are integer numbers (`Num`) or variables (`Var`).

```
case class Num (value : Int) extends Exp
case class Var (name : Idn) extends Exp
type Idn = String
```

A lambda expression (`Lam`) creates an anonymous function with a
single formal argument of a given type and a body expression that may
refer to that argument. The variable name does not have to be unique.

```
case class Lam (name : Idn, tipe : Type, body : Exp) extends Exp
```

An application (`App`) applies a function to an actual argument
expression.

```
case class App (l : Exp, r : Exp) extends Exp
```

Primitive binary operations are represented by `Opn` instances.

```
case class Opn (op : Op, left : Exp, right : Exp) extends Exp
```

For some evaluation mechanisms, a single substitution is represented
explicitly as a `Let` instance.  In a `Let` we are substituting all
occurrences of the variable `name` in `body` with `exp`.

```
case class Let (name : Idn, tipe : Type, exp : Exp, body : Exp) extends Exp
```

Other evaluation mechanisms use parallel subsitutions, represented by
`Letp` instances. All of the bindings in a `Letp` are applied in
parallel to the `body`.

```
case class Letp (bindings : List[Bind], body : Exp) extends Exp
case class Bind (name : Idn, tipe : Type, exp : Exp)
```

## Types

File: [org.bitbucket.inkytonik.kiama.example.lambda2.LambdaTree.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/LambdaTree.scala)

It is possible to support a large variety of primitive types, but
adding new ones does not really add anything meaningful to the
example. Hence, we only support a single primitive integer type.

```
abstract class Type
case object IntType extends Type
```

Functions are represented by a `FunType` that gives the type of the
function argument and the type of its result.

```
case class FunType (arg : Type, res : Type) extends Type
```

## Operations

File: [org.bitbucket.inkytonik.kiama.example.lambda2.LambdaTree.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/LambdaTree.scala)

Primitive binary operations are represented by `Op` instances. Each
`Op` subclass provides an evaluation method to actually run the
primitive on two integer values.

```
abstract class Op {
    def eval (l : Int, r : Int) : Int
}
```

As for types, we could easily support a large variety of primitive
operations, but we choose to just support a couple to keep things
simple.

```
case object AddOp extends Op {
    def eval (l : Int, r : Int) = l + r
}

case object SubOp extends Op {
    def eval (l : Int, r : Int) = l - r
}
```

## Pretty printing

File: [org.bitbucket.inkytonik.kiama.example.lambda2.PrettyPrinter.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/PrettyPrinter.scala)

The result of an expression evaluation is pretty printed using a
straight-forward functional application of Kiama's
[pretty-printing library](PrettyPrinting.md).

```
object PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

    import AST._

    /**
     * Return a pretty-printed version of an expression.
     */
    def pretty (t : Exp) : String =
        super.pretty (show (t))

    /**
     * Return a pretty-printed version of a type.
     */
    def pretty (t : Type) : String =
        super.pretty (showtype (t))

    /**
     * Convert an expression node to a pretty-printing document in
     * fully-parenthesised style.
     */
    private def show (t : Exp) : Doc =
        t match {
            case Num (d)       => value (d)
            case Var (i)       => i
            case Lam (i, t, e) => parens ('\\' <> text (i) <>
                                          showtypedecl (t) <+> '.' <+>
                                          group (nest (show (e))))
            case App (e1, e2)  => parens (show (e1) <+> show (e2))

            case Opn (AddOp, l, r) => showbin (l, "+", r)
            case Opn (SubOp, l, r) => showbin (l, "-", r)

            case Let (i, t, e1, e2) =>
                parens ("let" <+> text (i) <> showtypedecl (t) <+> '=' <>
                        nest (line <> show (e1)) <+> "in" <>
                        nest (line <> show (e2)))
            case Letp (bs, e) =>
                parens ("letp" <+>
                        nest (line <> vsep (bs.map (b => text (b.i) <+> '=' <+> show (b.e)))) <+>
                        "in" <>
                        nest (line <> show (e)))
        }

    /**
     * Return a pretty-printing document for an instance of a type declaration.
     */
    private def showtypedecl (t : Type) : Doc =
        if (t == null)
            empty
        else
            space <> ':' <+> showtype (t)

    /**
     * Return a pretty-printing document for an instance of a type.
     */
    private def showtype (t : Type) : Doc =
        t match {
            case IntType          => "Int"
            case FunType (t1, t2) => showtype (t1) <+> "->" <+> showtype (t2)
        }

    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    private def showbin (l : Exp, op : String, r : Exp) : Doc =
        parens (show (l) <+> op <+> show (r))

}
```

## Parser

File: [org.bitbucket.inkytonik.kiama.example.lambda2.SyntaxAnalyser.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/SyntaxAnalyser.scala)

The [parser](Parsing.md) is a simple application of Scala's [parser combinators](ParserCombs.md).
(See the [Imperative](Imperative.md) example for more explanation of a similar parser.)  Types are optional
in the lambda expressions, controlled by the setting of `typecheck` which can be set by
the user in the REPL.

```
lazy val start : PackratParser[Exp] =
    exp

lazy val exp : PackratParser[Exp] =
    "\\" ~> idn ~ (":" ~> itype) ~ ("." ~> exp) ^^ Lam |
    exp2

def itype =
    if (typecheck) (":" ~> ttype) else ("" ^^^ null)

lazy val exp2 : PackratParser[Exp] =
    exp2 ~ op ~ exp1 ^^ Opn |
    exp1

lazy val exp1 : PackratParser[Exp] =
    exp1 ~ exp0 ^^ App |
    exp0

lazy val exp0 : PackratParser[Exp] =
    number | idn ^^ Var | "(" ~> exp <~ ")"

lazy val ttype : PackratParser[Type] =
    ttype0 ~ ("->" ~> ttype) ^^ FunType |
    ttype0

lazy val ttype0 : PackratParser[Type] =
    "Int" ^^^ IntType |
    "(" ~> ttype <~ ")"

lazy val op =
    "+" ^^^ AddOp |
    "-" ^^^ SubOp

lazy val idn =
    "[a-zA-Z][a-zA-Z0-9]*".r

lazy val number =
    "[0-9]+".r ^^ (s => Num (s.toInt))
```

## Name and type analysis

File: [org.bitbucket.inkytonik.kiama.example.lambda.Analyser.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Analyser.scala)

After an abstract syntax tree has been built, we check it for semantic
correctness. In this example that means we make sure that each
variable use has a definition as a formal argument in an enclosing
lambda expression. Also, we make sure that the types are used
correctly. For example, values that are applied have to be functions
and the actual argument in the application has to have a type that is
the same as the formal argument type of the function being applied.

The example code provides two schemes for achieving this kind of
semantic analysis, which are described in the next two sections.

## An Environment-based Analysis

File: [org.bitbucket.inkytonik.kiama.example.lambda.Analyser.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Analyser.scala)

A classical solution to this kind of analysis problem is to construct
an _environment_ (usually called a _symbol table_ in a compiler
setting) that contains information about the variables that have been
declared and their types.

We define the `env` attribute of an expression to be a list of the
variables that are in scope at that expression and their types. `env`
depends on the context of the expression, so it is defined by a
[childAttr](Attribution.md#markdown-header-attribute-definitions) definition.

```
val env : Exp => List[(Idn,Type)] =
    childAttr {
        case _ => {
            ... cases ...
        }
    }
```

If we are at the root of the tree, then no variables are in scope.
In other words, there are no pre-defined variables.

```
case null => List ()
```

Immediately inside a lambda expression we know that the formal
argument of the lambda expression is in scope, in addition to all of
the things that are in scope from expressions that enclose the lambda
expression.

```
case p @ Lam (x, t, _) => (x,t) :: p->env
```

Finally, if neither of the first two cases apply, then the parent
doesn't matter and we just propagate the request up the tree.

```
case p : Exp => p->env
```

These definitions mean that a request for `env` at a particular node
will be answered by looking upward in the tree collecting one variable
at each lambda expression that is encountered on the way to the root.

With `env` in hand, we can define an attribute `tipe` (since `type` is
a Scala keyword) that returns the type of an expression. We use the
AST node type `Type` to represent types rather than create another
representation.

```
val tipe : Exp => Type =
    attr {
        ... cases ...
    }
```

A number is always of integer type.

```
case Num (_) => IntType
```

A lambda expression is always of function type, where the argument
type is given by the lambda expression itself (`t`) and the result
type is the type of the body expression (`e`).

```
case Lam (_, t, e) => FunType (t, e->tipe)
```

The result of a primitive operation is always of integer type, but the
operation is only legal if the two arguments are integers.

```
case Opn (op, e1, e2) => if (e1->tipe != IntType)
                             message (e1, "expected Int, found " + (e1->tipe))
                         if (e2->tipe != IntType)
                             message (e2, "expected Int, found " + (e2->tipe))
                         IntType
```

For an application there are three cases to consider. The correct case
is when the type of the expression (`e1->tipe`) is a function and its
argument type (`t1`) is the same as the type of the argument
(`e2->tipe`). In this case the type of the application is the result
type of the function (`t2`). The other two cases signal errors.
First, the type of the actual argument may not be correct. Second, the
expression being applied might not be a function at all.

```
case App (e1, e2) => e1->tipe match {
                         case FunType (t1, t2) if t1 == e2->tipe => t2
                         case FunType (t1, t2) =>
                             message (e2, "expected " + t1 + ", found " + (e2->tipe))
                             IntType
                         case _ =>
                             message (e1, "application of non-function")
                             IntType
                     }
```

Finally, the case that requires `env`. If the expression we are
examining is a variable reference, we search for that variable in the
environment at that expression. If we find it, then we use the type of
the binding that we found. Otherwise, we report an unknown variable.

```
case e @ Var (x) => (e->env).find { case (y,_) => x == y } match {
                        case Some ((_, t)) => t
                        case None =>
                            message (e, "'" + x + "' unknown")
                            IntType
                    }
```

## A Reference-based Analysis

File: [org.bitbucket.inkytonik.kiama.example.lambda.Analyser.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda/Analyser.scala)

Instead of building a separate environment structure to keep track of
variables that are in scope, we can use the tree itself. In this
approach we resolve variable references by searching the tree for the
relevant binding lambda expression and, if it is found, we just use a
reference to that node instead of to an entry in an environment.

It is easiest to understand if we first look at the variable reference
case for the `tipe` attribute. (All of the other cases are the same.)
In this version we have a parameterised `lookup` attribute whose
parameter is the variable for which we are looking. `lookup`s value is
an `Option` that is either some lambda expression node in which the
variable we are seeking is declared, or `None`, representing that
there is no such declaration. In the former case, we can just extract
the type from the lambda expression. In the latter, we signal an
error.

```
case e @ Var (x) => (e->lookup (x)) match {
                        case Some (Lam (_, t, _)) => t
                        case None =>
                            message (e, "'" + x + "' unknown")
                            IntType
                    }
```

`lookup` is also defined easily as a search up the tree, similar to
`env` except that it doesn't accumulate bindings, it just returns the
relevant one if it finds it. The first case is when we have found a
lambda expression that declares the variable we are looking for. The
second case is when we get to the root without having found a
declaration. The third case deals with all other situations by just
propagating the request to the parent node.

```
def lookup (name : Idn) : Exp => Option[Lam] =
    attr {
        case e @ Lam (x, t, _) if x == name => Some (e)
        case e if e isRoot                  => None
        case e                              => e.parent[Exp]->lookup (name)
    }
```

## Evaluation mechanisms

File: [org.bitbucket.inkytonik.kiama.example.lambda2.Evaluators.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/Evaluators.scala)

The example includes a number of different evaluation mechanisms
defined by [rewrite rules](Rewriting.md). The evaluation strategies used
are heavily based on the Stratego ones given in
[Building Interpreters with Rewriting Strategies](http://doi.acm.org/10.1145/291251.289425),
Eelco Dolstra and Eelco Visser, LDTA 2002.  The following sections give an overview
of the Kiama implementation but the paper should be consulted for full details
on how they work.

## Repeated reduction with meta-level substitution

File: [org.bitbucket.inkytonik.kiama.example.lambda2.Reduce.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/Reduce.scala)

The standard evaluation rule for lambda calculus is _beta reduction_ which we can
write as follows, given a function that performs variable substitution.

```
lazy val beta =
    rule {
        case App (Lam (x, _, e1), e2) => substitute (x, e2, e1)
    }
```

Substitution can be coded easily as a regular Scala function. In the
second case, an auxiliary function `freshvar` is used to come up with
a variable name that has not been used elsewhere.

```
def substitute (x : Idn, e2: Exp, e1 : Exp) : Exp =
    e1 match {
        case Var (y) if x == y =>
            e2
        case Lam (y, t, e3) =>
            val z = freshvar ()
            Lam (z, t, substitute (x, e2, substitute (y, Var (z), e3)))
        case App (l, r) =>
            App (substitute (x, e2, l), substitute (x, e2, r))
        case Opn (op, l, r) =>
            Opn (op, substitute (x, e2, l), substitute (x, e2, r))
        case e =>
            e
    }
```

We also need to be able to evaluate primitive operations, for which we
just delegate to the operation.

```
lazy val arithop =
    rule {
        case Opn (op, Num (l), Num (r)) => Num (op.eval (l, r))
    }
```

We put this all together into a single strategy that repeatedly tries
to apply `beta` and `arithop` to the expression being evaluated,
stopping when neither applies to any (sub)expression.

```
lazy val s =
    reduce (beta + arithop)
```

Finally, we define `eval` that applies `s` to a particular expression.

```
def eval (exp : Exp) : Exp =
    rewrite (s) (exp)
```

## Repeated reduction with explicit substitution

File: [org.bitbucket.inkytonik.kiama.example.lambda2.ReduceSubst.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/ReduceSubst.scala)

Instead of encoding substitution separately, we can incorporate it
into the language and the rewrite rules by using `Let` constructs to
represents places where substitutions need to be applied. Beta
reduction then becomes simply an introduction of a substitution.

```
override lazy val beta =
    rule {
        case App (Lam (x, t, e1), e2) => Let (x, t, e2, e1)
    }
```

A suite of new rules actually perform substitutions in different cases.

```
lazy val subsNum =
    rule {
        case Let (_, _, _, e : Num) => e
    }

lazy val subsVar =
    rule {
        case Let (x, _, e, Var (y)) if x == y => e
        case Let (_, _, _, v : Var)           => v
    }

lazy val subsApp =
    rule {
        case Let (x, t, e, App (e1, e2)) =>
            App (Let (x, t, e, e1), Let (x, t, e, e2))
    }

lazy val subsLam =
    rule {
        case Let (x, t1, e1, Lam (y, t2, e2)) if x == y =>
            Lam (y, t2, e2)
        case Let (x, t1, e1, Lam (y, t2, e2)) =>
            val z = freshvar ()
            Lam (z, t2, Let (x, t1, e1, Let (y, t2, Var (z), e2)))
    }

lazy val subsOpn =
    rule {
        case Let (x, t, e1, Opn (op, e2, e3)) =>
            Opn (op, Let (x, t, e1, e2), Let (x, t, e1, e3))
    }
```

We combine everything in a single evaluation strategy, reusing `arithop`
from the previous section.

```
lazy val lambda =
    beta + arithop + subsNum + subsVar + subsApp + subsLam + subsOpn
```

Finally, evaluation is just repeated reduction using the `lambda` rule.

```
override lazy val s =
    reduce (lambda)
```

## Innermost evaluation

File: [org.bitbucket.inkytonik.kiama.example.lambda2.InnermostSubst.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/InnermostSubst.scala)

An alternative to `reduce` is to explicitly use an `innermost`
strategy so that the order of evaluation is more precisely defined. In
some cases, this can reduce the amount of work that has to be done.

We just need to redefine `s`.  All of the other strategies stay
the same from the previous section.

```
override lazy val s : Strategy =
    innermost (lambda)
```

The Kiama library has an `innermost` strategy which will do here, but it
is more efficient to remember the results of reductions so that they do
not have to be done again.  This version uses Kiama's `memo` combinator
to _memoise_ the results.

```
def innermost (s : => Strategy) : Strategy =
    memo (all (innermost (s) <* attempt (s <* innermost (s))))
```

## Eager evaluation

File: [org.bitbucket.inkytonik.kiama.example.lambda2.EagerSubst.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/EagerSubst.scala)

Some functional languages such as the ML family use an _eager
evaluation_ method where the arguments to functions are evaluated
before they are substituted into the body of the function. We can
implement that in the example by encoding a traversal that guides our
evaluation strategy to the parts that should be evaluated.

```
override lazy val s : Strategy =
    attempt (App (s, s) + Let (id, id, s, s) + Opn (id, s, s)) <*
    attempt (lambda <* s)
```

In this definition, the congruences will apply `s` to the
sub-expressions that should be evaluated at each point. Once
sub-expressions have been evaluated the evaluation rule will be
applied to the current expression root and then evaluation will be
repeated via the recursive call. `lambda` here is the same as in the
previous section.

Eager evaluation calls for evaluation to move
into applications, substitutions and primitive operations. Note that
the argument `e2` in an application is evaluated before `lambda` will
be applied to the application.

## Lazy evaluation

File: [org.bitbucket.inkytonik.kiama.example.lambda2.LazySubst.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/LazySubst.scala)

Instead of evaluating function arguments before substitution, we can
use _lazy evaluation_ where arguments are only evaluated if and when
they are needed. In the example this is easy to achieve, just by
replacing the congruences with versions that do not evaluate function
arguments or the bound expressions in substitutions.

```
override lazy val s : Strategy =
    attempt (App (s, id) + Let (id, id, id, s) + Opn (id, s, s)) <*
    attempt (lambda <* s)
```

## Parallel substitution

Files: [org.bitbucket.inkytonik.kiama.example.lambda2.ParEagerSubst.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/ParEagerSubst.scala)
[org.bitbucket.inkytonik.kiama.example.lambda2.ParLazySubst.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/ParLazySubst.scala)

Variants of eager and lazy evaluation can be written that manage
substitutions in parallel rather than in sequence as in the earlier
sections.  Since this page is long enough already, we direct the
reader to the source to see these versions work.  As well as the
two linked here, there are versions of the lazy parallel evaluator
that share and update results as evaluation proceeds.

## Running

File: [org.bitbucket.inkytonik.kiama.example.lambda2.Lambda.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/Lambda.scala)

The example code includes a [read-eval-print loop](ReadEvalPrintLoops.md)
that reads an expression from the user, parses the expression,
performs semantic analysis on the tree and then evaluates it.
Different evaluation mechanisms can be chosen using the `:eval`
command in the REPL.

To run the read-eval-print loop:

```
sbt 'main org.bitbucket.inkytonik.kiama.example.lambda2.Lambda'
Enter lambda calculus expressions for evaluation.
lambda> (\x : Int . 99) 42
99
lambda2> (\x : Int . 3 + 4)
(\x : Int . 7)
lambda2> :eval
Available evaluation mechanisms:
  reduce (current)
  reducesubst
  innermostsubst
  eagersubst
  lazysubst
  pareagersubst
  parlazysubst
  parlazyshare
  parlazyupdate
lambda2> :eval lazysubst
lambda2> (\x : Int . 3 + 4) 99
7
lambda2> (\x : Int . 3 + 4)
(\x : Int . (3 + 4))
lambda2> (\x : Int . y) 42
1.13: 'y' unknown
lambda2> (\x : Int . x 42) 99
1.13: application of non-function
lambda2>
```

## Tests

File: [org.bitbucket.inkytonik.kiama.example.lambda2.LambdaTests.scala](https://bitbucket.org/inkytonik/kiama/src/master/library/src/test/scala/org/bitbucket/inkytonik/kiama/example/lambda2/LambdaTests.scala)

Some simple tests of lambda calculus evaluation using all of the strategies.

```
sbt 'test-only org.bitbucket.inkytonik.kiama.example.lambda2.LambdaTests'
```

Up: [Examples](Examples.md), Prev: [Lambda](Lambda.md), Next: [Oberon0](Oberon0.md)
