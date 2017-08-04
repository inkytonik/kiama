# Constructing parsers using combinators

Up: [Parsing](Parsing.md), Prev: [Parser input](ParserInput.md)

Like any combinator-based approach to parsing, Scala's library starts
with simple parsers and uses functions to combine them into more
complicated ones. The most common ways to construct parsers are
covered here. See the Kiama library API documentation for `Parsers`
for a complete list.

See also [Parsing](Parsing.md) and [ParserInput](ParserInput.md) for general information about
defining and using parser combinators.

## Simple parsers

The simplest way to create a parser is to build one that doesn't
consume any input. `success` produces a parser that always succeeds
with a given result value. `failure` and `error` produce parsers that always
fail or error with a given message.

```
def success[T] (v : => T) : Parser[T]
def failure (message : String) : Parser[Nothing]
def error (message : String) : Parser[Nothing]
```

`success` is not very useful, but the other two can be used to produce
useful error messages if other input-consuming parsers have failed.
See below for an example.

The simplest way to create a parser that consumes some input is from
an existing input character. `elem` takes a character `ch` and
returns a parser that succeeds only if `ch` is the next input element.

```
implicit def elem (ch : Char) : Parser[Char]
```

If any input element is legal instead of just a single one,
the `any` combinator can be used.

```
def any : Parser[Char]
```

## Tokens

Since Scala `RegexParsers` operate at the character level, a separate scanner
is not needed to recognise lexical tokens to pass to the parser. This
approach is often called [scannerless parsing](http://en.wikipedia.org/wiki/Scannerless_parsing).

Even if tokens are not passed around, it is often still useful to use
abstractions to describe the level of structure immediately above the
character level.

`literal` converts a string into a parser that recognises just that string
followed by layout. For example, `literal` can be used to parse keywords or
multi-character operator symbols.

```
implicit def literal (s : String) : Parser[String]
```

Since literal is `implicit` if a parser is required you can just give a string.

```
lazy val whileParser : Parser[String] = "while"
```

Note that the values returned by literal parsers are of type `String`
since a sequence of characters is recognised. `literal` will also skip any white
space that is present, before trying to parse the literal.  You can
customise the definition of white space by overriding the `whiteSpace`
regular expression from `Parsers`.

A cousin of `literal` is `regex` which creates a parser that recognises input that matches a
given regular expression. For example, a decimal number might be
specified to be a sequence of one or more digits.  White space is also
skipped. `regex` is also implicit so it suffices to provide a regular expression object.

```
implicit def regex (r: Regex) : Parser[String]

lazy val decimal : Parser[String] = """\d+""".r
```

Recall that `"foo".r` converts a string into a regular expression object.
The `"""...""".r` form is Scala's way of obtaining a regular
expression from a string in which backslash escape sequences are not
interpreted.

## Identifiers

A common token type for many languages is the _identifier_, a name that
represents something. It is easy to use `regex` to define the form that
identifiers can take. For example,

```
lazy val identifier : Parser[String] = "[a-zA-Z]+".r
```

When parsing some languages, care must be taken to avoid clashes between
identifiers and keywords. For example, when parsing C, the character
sequences `if` and `while` should be recognised as keywords, not as
identifiers, even though they match the definition of identifiers.

One approach for avoiding problems in this kind of case is to write
the parser definition for identifiers to rule out the possibility
of matching a keyword. We still use keyword literals where they are
needed in other parser definitions, as described in the previous
section. We just augment the `identifier` definition with an extra
check, using the predicate combinator `not`.

A parser `not (p)` flips the success and failure of the parser `p`
and doesn't consume any input. Thus, something of this form can be
used to test whether the input can be parsed by `p` and only move
to subsequent parsers if it cannot. We can use this idea to avoid
recognising identifiers that look like keywords.

A first attempt at a solution might look like this:

```
lazy val keyword : Parser[String] = "if"
lazy val identifier = not (keyword) ~> """[a-zA-Z]+""".r
```

Here we are defining the form of keywords with a separate parser,
then using it in the `identifier` definition to rule those cases
out. The `keyword` parser can easily be extended to have cases for
all of the keywords.

The problem with this first attempt is that it doesn't allow
identifiers of the form `iffoo`, i.e., where the prefix matches
a keyword. We need the `keyword` parser to ensure that the following
character can't continue the sequence as an identifier. Here is
one option:

```
lazy val keyword : Parser[String] = "if[^a-zA-Z]"
```

Again, this can be extended to multiple keywords. `Parsers`
provides an operation called `keywords` to make it easy to apply this approach.

This approach is not without flaws. It requires the keywords to be
repeated, once in the place where they are actually parsed and once
in the `keyword` parser. Also, the regular expression that defines
the continuation as an identifier must be kept consistent with the
regular expression used in the `identifier` definition.

## Sequencing

Various sequencing combinators can be used to form larger structural
units than single tokens. The basic sequencing combinator is the `~`
method where `p ~ q` recognises what `p` does, and if `p` succeeds,
then recognises what `q` does.

```
abstract class Parser[+T] ... {
    def ~[U] (q : => Parser[U]) : Parser[~[T, U]]
}
```

The result of a `p ~ q` parser is a tuple formed using the `~` case
class, containing the result of `p` as the first component and the
result of `q` as the second component.

```
case class ~[+U,+V] (_1 : U, _2 : V)
```

On some occasions a sequence needs to be recognised, but only the
result of one of the parsers is of interest. The `~>` and `<~` methods
can be used in this situation. They discard the result of the parser
that is not pointed to by the `>` or `<`. Thus `p ~> q` only returns
the result of `q` and `p <~ q` only returns the result of `p`.

```
abstract class Parser[+T] ... {
    def ~>[U] (q : => Parser[U]) : Parser[U]
    def <~[U] (q : => Parser[U]) : Parser[T]
}
```

There is also a non-backtracking sequence combinator called `~/` with
variants `~/>` and `<~/`.

The following parsers use the sequencing methods to recognise
assignment statements and while loops, discarding the results of
parsing the keywords and special symbols.

```
lazy val asgnStmt = idn ~ ("=" ~> exp) <~ ";"
lazy val whileStmt = ("while" ~> "(" ~> exp <~ ")") ~ stmt
```

## Alternation

Most languages have alternate forms for some constructs, such as
different forms of statement. The alternation method `|` is used to
specify these alternatives.

```
abstract class Parser[+T] ... {
    def |[U >: T] (q : => Parser[U]) : Parser[U]
}
```

A parser `p | q` will first try to recognise what `p` does. If `p`
succeeds, then its result is returned. If `p` fails, then `q` is
tried. In contrast to alternation in general context-free grammars,
this is an ordered choice: always `p` then `q`. For this reason,
alternatives that have an overlapping prefix should first list the one
that will possibly match the most. For example, to recognise
conditional statements, the if-then-else case should come first.

```
lazy val if_statement =
    "if" ~> expression ~ ("then" ~> statements) ~ ("else" ~> statements) |
    "if" ~> expression ~ ("then" ~> statements)
```

A `failure` parser can be used as the last alternative to provide a
context-specific error message if none of the earlier alternatives
succeed.

```
lazy val statement =
    ... alternatives for statements ... |
    failure ("statement expected")
```

## Optional constructs

Optional constructs are expressed using the `opt` combinator or the
`?` method, where `p? == opt (p)`. If `p` succeeds with value `v`, the
result of `opt (p)` is `Some (v)`, otherwise it is `None`.

```
def opt[T] (p : => Parser[T]) : Parser[Option[T]]

abstract class Parser[+T] ... {
    def ? : Parser[Option[T]]
}
```

For example, an optional `extends` clause in a class definition might
be defined as follows.

```
lazy val class_decl = "class" ~> IDENTIFIER ~ (xtends?) ~ block
lazy val xtends     = "extends" ~> IDENTIFIER
```

## Repeated constructs

A collection of combinators and methods facilitate specification of
repeated constructs.

| `rep (p)` | repeat `p` zero or more times |
|:----------------|:------------------------------|
| `repsep (p, sep)` | repeat `p` zero or more times separated by `sep` |
| `rep1 (p)` | repeat `p` one or more times  |
| `rep1sep (p, sep)` | repeat `p` one or more times separated by `sep` |

The parsers created by these combinators return vectors of the result
produced by `p`. In the separator versions, the results of `sep` are
discarded.

```
def rep[T] (p : => Parser[T]) : Parser[Vector[T]]
def repsep[T,U] (p : => Parser[T], sep: => Parser[U]) : Parser[Vector[T]]
def rep1[T] (p : => Parser[T]) : Parser[Vector[T]]
def rep1sep[T,U] (p : => Parser[T], sep: => Parser[U]) : Parser[Vector[T]]
```

Use the `ListParsers` class instead of `Parsers` to have these
methods return lists instead of vectors.

The following examples show how repetition is used to parse statement
sequences and comma-separated expression lists.

```
lazy val stmtseq = "{" ~> rep (stmt) <~ "}"
lazy val explist = repsep (exp, ",")
```

## Values returned by parsers

The combinators described so far are sufficient to recognise most
structures. Often, however, the value returned by a parser is not
exactly what is needed. For example, it is common to require an
abstract syntax tree (AST) from a parse, rather than generic
structures such as tuples and lists.

The `^^` method is used to transform the result of a successful parse.
If `p` is a parser, `p ^^ f` produces a parser that recognises what
`p` does, and if `p` is successful, applies `f` to the result of `p`.

```
abstract class Parser[+T] ... {
    def ^^[U] (f : T => U) : Parser[U]
}
```

The following extensions of examples given earlier show how `^^` is
used to construct AST nodes for successfully parsed constructs.

```
case class Seqn (ss : Seq[Stmt]) extends Stmt
lazy val stmtseq : Parser[Seqn] =
    "{" ~> (stmt*) <~ "}" ^^ Seqn

case class Asgn (s : Idn, e : Exp) extends Stmt
lazy val asgnStmt : Parser[Asgn] =
    idn ~ ("=" ~> exp) <~ ";" ^^
        { case s ~ e => Asgn (s, e) }

case class While (e : Exp, b : Stmt) extends Stmt
lazy val whileStmt : Parser[While] =
    ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^
        { case e ~ b => While (e, b) }

case class Num (d : Double) extends Exp
lazy val double : Parser[Num] =
    ("""[0-9]+\.[0-9]+""") ^^ { case s => Num (s.toDouble) }
```

In the first example, the `Seqn` constructor can be used directly
since the parser produces a single value. In the others, a pattern
matching anonymous function is used to decompose the tuple returned by
the parser so that its components can be used to construct an
appropriate AST node.

The `^^^` method can be used to return a constant value from a parser,
which is sometimes useful for constant leaf nodes in the AST.

The classes `Seqn`, `Asgn`, `While` and `Num` are case classes, which
makes it easy to create instances as shown. It is tempting to use case
objects for constant nodes, but this practice should be avoided when
building an AST, since the resulting structure will be a graph, not a
tree because the case object leaves will be shared.

## Convenient value construction conversions

In the second example from the previous section
we must decompose the pair that is built by the underlying parser,
only to pass each component to the `Asgn` constructor. This pattern
is very common, so Kiama provides implicit conversions and the
pattern matching can be omitted. For example, for the pair case we
have

```
implicit def constToTupleFunction2[A,B,R] (r : (A,B) => R) : (A ~ B) => R = {
    case a ~ b =>
        r (a, b)
}
```

The example can be simplified using the conversion since it takes
care of converting the `Asgn` constructor into something that can
take the pair produced by the parser.

```
lazy val asgnStmt : Parser[Asgn] =
    idn ~ ("=" ~> exp) <~ ";" ^^ Asgn
```

Similar conversions are provided for up to six arguments.

Similarly, it is sometimes useful to return a regular Scala tuple
from a parser that makes a _tilde_ tuple. Kiama provides implicit
conversions up to tuples of size six for this purpose.

```
implicit def parseResultToTuple2[A,B] (p : Parser[A ~ B]) : PackratParser[(A,B)] =
    p ^^ { case a ~ b => (a,b) }
```

## Testing the input with parsers

Sometimes it is useful to be able to test whether a parser applies
without actually consuming any input. The `guard` combinator constructs
a parser that has the same behaviour as its argument parser, but
leaves the input position unchanged. `not` is similar, but reverses
the test. `not (p)` succeeds with `()` if `p` fails, otherwise it
fails, in both cases without changing the input.

```
def guard[T] (p : => Parser[T]) : Parser[T]
def not[T] (p : => Parser[T]) : Parser[Unit]
```

## Left recursion and memoisation

The parsing combinators as defined so far do not support left
recursion. Thus a parser definition such as

```
lazy val exp = exp ~ ("+" ~> term) | exp ~ ("-" ~> term) | term
```

will not work since `exp` calls itself without consuming any input.

The usual solution to this problem is to rewrite the `exp` definition
using repetition to avoid the left recursion. Unfortunately, this
approach obscures the structure of the grammar and makes AST
construction more complicated. Kiama provides an alternative via
_packrat_ or _memoising parsers_ represented by the type
`PackratParser[T]` that extends `Parser[T]`.

A `PackratParser` wraps the actual parsing mechanism so that left
recursion is supported and the results of parses are remembered. Where
a memoising parser is required, it is necessary to explicitly declare
the parser to be a `PackratParser`. Therefore, a full, left recursive
definition of simple expressions might be written as follows.

```
lazy val exp : PackratParser[Exp] =
    exp ~ ("+" ~> term) ^^ { case l ~ r => Add (l, r) } |
    exp ~ ("-" ~> term) ^^ { case l ~ r => Sub (l, r) } |
    term

lazy val term : PackratParser[Exp] =
    term ~ ("*" ~> factor) ^^ { case l ~ r => Mul (l, r) } |
    term ~ ("/" ~> factor) ^^ { case l ~ r => Div (l, r) } |
    factor

lazy val factor : PackratParser[Exp] =
    integer | variable | "(" ~> exp <~ ")"
```

The memoisation performed by these parsers is important when
backtracking occurs. For example, in the `exp` parser, if the `exp` at
the beginning of the first alternative succeeds but the `+` fails,
then the second alternative will be tried. Since `exp` has already
succeeded at the initial input position and the result memoised, the
`exp` at the beginning of the second alternative will succeed
immediately reusing the result without needing to reparse the input.
This reuse avoids exponential behaviour where text is repeatedly
parsed by the same parser. The cost is some storage for memoisation.

Note that the memoisation described here is provided by the
PackratParser class. If you proceed as shown above, the "top-level"
parsers `exp`, `term` and `factor` will memoise since they have
explicit type annotations. However, the intermediate parser values
(such as the three alternatives that make up `exp` or their
constituents) will be `Parser` values, and hence will not memoise. In
practice, this difference may not be important, but it does mean that
parsers written in this way do not have the linear storage properties
of true packrat parsers.

## Positions

The combinators discussed here will automatically keep track of
position infirmation and store it using the `positions` argument
to `Parsers`.

It is possible to query this position information later.
For example, if `n` is a node that was created by one of these
parsers then `positions.getStart (n)` is the (optional) position
encoding where the input corresponding to `n` started.
Similarly, for `getFinish`.

See the API documentation for `Positions` for more information.

Kiama's [Messaging](Messaging.md) module is designed to work with
values that have positions.
All you need to do is pass such a value
to the `message` method and the position information will be used
automatically.

## Other combinators

Kiama's `wrap` combinator wraps a parser `p` so that its value (of
type `T`) is post-processed by a function `f`. `f` can either produce
a value of type `U`, then that value becomes the value of the wrapped
parser. Otherwise, `f` can return a string which used as the message
for the failure of the wrapped parser.

```
def wrap[T,U] (p : => Parser[T], f : T => Either[U,String]) : Parser[U]
```

`wrap` is most useful for performing a check on the value produced
by a parser before deciding whether the parse has succeeded or not.
For example, Kiama provides the `constrainedInt` parser which parses
a string of digits but only succeeds if the numeric value will fit
into an `Int` (as determined by the operation `stringToInt`).

Up: [Parsing](Parsing.md), Prev: [Parser input](ParserInput.md)
