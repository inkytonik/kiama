# Example: Simple imperative language

Up: [Examples](Examples.md), Prev: [Dataflow](Dataflow.md), Next: [Lambda](Lambda.md)

This example contains an abstract syntax tree structure for a simple
imperative programming language, a parser that creates instances of
the abstract syntax from textual representations of imperative
language programs and a [read-eval-print loop](ReadEvalPrintLoops.md) that
inputs programs and prints their tree representations. The imperative
language is mostly used to implement tests of the Kiama rewriting
library, so there is also quite a bit of test-related scaffolding.

## Abstract syntax

File: [org.kiama.example.imperative.Imperative.scala](https://code.google.com/p/kiama/source/browse/library/src/org/kiama/example/imperative/Imperative.scala)

Programs for this example consist of a single statement of which
some standard varieties are available.

```
abstract class Stm
case class Asgn (s : Idn, e : Exp) extends Stmt
case class While (e : Exp, b : Stmt) extends Stmt
case class Seqn (ss : Seq[Stmt]) extends Stmt
case class Null () extends Stmt
```

Expressions are also standard. Binary expressions are factored out to
allow some common behaviour to be defined in one place.

```
abstract class Exp

case class Num (d : Double) extends Exp
case class Var (s : Idn) extends Exp
case class Neg (e : Exp) extends Exp

abstract class Binary (l : Exp, r : Exp) extends Exp
case class Add (l : Exp, r : Exp) extends Binary (l, r)
case class Sub (l : Exp, r : Exp) extends Binary (l, r)
case class Mul (l : Exp, r : Exp) extends Binary (l, r)
case class Div (l : Exp, r : Exp) extends Binary (l, r)
```

Finally, identifiers are just represented by strings.

```
type Idn = String
```

## Parsing

File: [org.kiama.example.imperative.Imperative.scala](https://code.google.com/p/kiama/source/browse/library/src/org/kiama/example/imperative/Imperative.scala)

The parser is written using Kiama's [parsing combinator library](Parsing.md).
Lazy values are used to enable the parsers to be defined in any order.

A whole parse is a parse of a single statement that consumes all of the
input.

A statement is parsed as one of four alternatives corresponding to the
different statement types.

Expressions are a bit trickier since the easiest encoding involves
left recursion. To make this work we use Scala's `PackratParser`
extension of standard parsers so that memoisation and packrat parsing
techniques for automatically dealing with the left recursion can be
used transparently. Other than the special type, the expression
productions encode the precedence and associativity of the operators
in a standard way.

```
lazy val parse : PackratParser[Stmt] =
    phrase (stmt)

lazy val stmt : PackratParser[Stmt] =
    ";" ^^^ Null () | sequence | asgnStmt | whileStmt

lazy val asgnStmt =
    idn ~ ("=" ~> exp) <~ ";" ^^ Asgn

lazy val whileStmt =
    ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ While

lazy val sequence =
    "{" ~> (stmt*) <~ "}" ^^ Seqn

lazy val exp : PackratParser[Exp] =
    exp ~ ("+" ~> term) ^^ Add |
    exp ~ ("-" ~> term) ^^ Sub |
    term

lazy val term : PackratParser[Exp] =
    term ~ ("*" ~> factor) ^^ Mul |
    term ~ ("/" ~> factor) ^^ Div |
    factor

lazy val factor : PackratParser[Exp] =
    double | integer | variable | "-" ~> exp | "(" ~> exp <~ ")"
```

Finally, we define the leaf forms of expression.  Numbers are
straightforward.

```
lazy val double : PackratParser[Num] =
    """[0-9]+\.[0-9]+""" ^^ (s => Num (s.toDouble))

lazy val integer : PackratParser[Num] =
    "[0-9]+".r ^^ (s => Num (s.toInt))
```

The remaining interesting detail is the use of a parsing predicate to
ensure that the `while` keyword is not accepted as a variable
identifier.

```
lazy val variable : PackratParser[Var] =
    idn ^^ Var

lazy val idn : PackratParser[String] =
    not (keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r

lazy val keyword : Parser[String] =
    "while"
```

## Pretty-printing

File: [org.kiama.example.imperative.PrettyPrinter.scala](https://code.google.com/p/kiama/source/browse/library/src/org/kiama/example/imperative/PrettyPrinter.scala)

The example includes a simple pretty-printer that uses Kiama's
[pretty-printing library](PrettyPrinting.md).  See the end of the
[pretty-printing library documentation](PrettyPrinting.md) for a
description of the imperative pretty printer.

## Read-eval-print loop

File: [org.kiama.example.imperative.Imperative.scala](https://code.google.com/p/kiama/source/browse/library/src/org/kiama/example/imperative/Imperative.scala)

Kiama provides some basic facilities to make it easy to define
[read-eval-print loops (REPLs)](ReadEvalPrintLoops.md). In the imperative
example the main REPL parses program text and prints the resulting
tree representations. (See [Running](#markdown-header-running) for an example execution.)

```
object Imperative extends ParsingREPL[Stmt] with Parser {

    override def setup { println ("Enter imperative language programs for parsing.") }
    override def prompt = "imperative> "

    def process (s : Stmt) {
        println (s)
    }

}
```

## Test scaffolding

File: [org.kiama.example.imperative.Imperative.scala](https://code.google.com/p/kiama/source/browse/library/src/org/kiama/example/imperative/Imperative.scala)

The main test scaffolding provided in this example is
[ScalaCheck](http://code.google.com/p/scalacheck/) support for
generating random program instances. The programs that are generated
are unlikely to be semantically correct, but serve as semi-realistic
trees for testing.

The generation code is a fairly standard use of ScalaCheck so we just
show a few representative examples here. The following three
generators produce random number expressions with a bias towards
integer ones.

```
val genInteger = for (i <- Gen.choose (1, 100)) yield Num (i)
val genDouble = for (i <- Gen.choose (1.0, 1000000.0)) yield Num (i)
val genNum = Gen.frequency ((3, genInteger), (1, genDouble))

implicit def arbNum : Arbitrary[Num] =
    Arbitrary (genNum)
```

`genAdd` produces addition expressions.

```
def genAdd (sz : Int) =
    for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Add (l, r)
```

Finally, `genAsgn` generates random assignment statements.

```
def genAsgn (sz : Int) =
    for { i <- genIdn; e <- genExp (sz-1) } yield Asgn (i, e)
```

There is also a REPL that generates random imperative programs and
prints them.  (See [Running](#markdown-header-running) for an example execution.)

```
object ImperativeGen extends GeneratingREPL[AST.Stmt] with Generator with PrettyPrinter {

    def generator = arbStmt

    override def process (s : AST.Stmt) {
        println (s)
        println (pretty (s))
    }

}
```

The code also contains some extra methods on the tree classes to
support semantic-based testing. The `vars` method returns a set of the
variables that are used in a construct. `value` computes the
arithmetic value of an expression, assuming that variables are not
zero (in fact, that they are always three). `divsbyzero` returns the
number of divisions by zero in an expression. `depth` and `intadds`
compute similar metrics on expressions.

## Running

File: [org.kiama.example.imperative.Imperative.scala](https://code.google.com/p/kiama/source/browse/library/src/org/kiama/example/imperative/Imperative.scala)

To run the read-eval-print loop to parse programs and print their tree representations:

```
sbt 'main org.kiama.example.imperative.Imperative'
Enter imperative language programs for parsing.
imperative> a = 4;
Asgn("a",Num(4.0))
imperative> { a = 0; while (a + 4) { a = a - 1; } }
Seqn(List(Asgn("a",Num(0.0)), While(Add(Var("a"),Num(4.0)),Seqn(List(Asgn("a",Sub(Var("a"),Num(1.0))))))))
imperative>
```

To run the read-eval-print loop that generates random programs:

```
sbt 'main org.kiama.example.imperative.ImperativeGen'
Each time you hit ENTER a new instance is generated and printed.
Hit ENTER to generate an instance:
Asgn("ymNmq",Mul(Div(Var("ymliy"),Num(74403.7187785454)),Num(10.0)))
ymNmq = ((ymliy / 74403.7187785454) * 10.0);
Hit ENTER to generate an instance:
```

## Tests

File: [org.kiama.rewriting.RewriterTests.scala](http://code.google.com/p/kiama/source/browse/library/src/org/kiama/rewriting/RewriterTests.scala)

A collection of tests that rewrite imperative language expressions and statements.

```
sbt 'test-only org.kiama.rewriting.RewriterTests'
```

File: [org.kiama.rewriting.UniplateTests.scala](http://code.google.com/p/kiama/source/browse/library/src/org/kiama/rewriting/UniplateTests.scala)

A collection of rewriting tests based on examples in a Haskell
Workshop 2007 paper about the
[UniPlate](http://community.haskell.org/~ndm/uniplate/) library.

```
sbt 'test-only org.kiama.rewriting.UniplateTests'
```

Up: [Examples](Examples.md), Prev: [Dataflow](Dataflow.md), Next: [Lambda](Lambda.md)
