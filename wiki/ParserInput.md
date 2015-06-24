# Parser basics, inputs and results

Up: [Parsing](Parsing.md), Next: ParserCombs

See also [Parsing](Parsing.md) and ParserCombs for general information about defining
and using parser combinators.

Most parsers for use with Kiama will operate on character input.
We recommend using the following two library traits.

```
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers
```

The `PackratParsers` trait gives you backtracking parsers that
_memoise_ their results, so that they allow unlimited lookahead
and performance that is linear in terms of the input size.  They
also allow left recursive productions to be directly encoded.
(`PackratParsers` extends a more standard `Parsers` trait that
uses less space than the packrat parsers because memoisation is
not done, but loses the linear performance and left recursion.)
`RegexParsers` provides facilities particular for parsers of
character input.

Thus, the usual structure for a parser is as follows.

```
trait Parser extends RegexParsers with PackratParsers {
    ... parser definitions ...
}
```

## Parser inputs

A parser processes input of type `Input` which is defined to be
[scala.util.parsing.input.Reader[Elem](http://www.scala-lang.org/docu/files/api/scala/util/parsing/input/Reader.html)]
where `Elem` is the type of the input elements to be processed.
`RegexParsers` defines `Elem` to be `Char`.

## Parser results

A parser that returns a value of type `T` on success is of type `Parser[T]`
which extends the function type `Input => ParseResult[T]`.
`ParseResult` has subclasses to represent the possible outcomes of
a parse.

```
case class Success[T] (result : T, next : Input) extends ParseResult[T]
abstract class NoSuccess(msg: String, next: Input) extends ParseResult[Nothing]
case class Failure (msg : String, next : Input) extends NoSuccess (msg, next)
case class Error (msg : String, next : Input) extends NoSuccess (msg, next)
```

In each cases, the `next` parameter represents the input remaining after
the parse. `Success.result` is the value produced by the successful
parse, whereas the `msg` field is the message resulting from a failed
parse.  A `Failure` represents a failure that can backtrack to retry
earlier parses, whereas an `Error` represents a final error.

## Running a parser

Since a parser is just a function, it can be applied directly to an
input to get a result. I.e., `p (in)` for a parser `p` and input `in`.

Invocation of a parser is also encapsulated by the `parse` function,
provided by `RegexParsers`.

```
def parse[T] (p : Parser[T], in: Reader[Char]) : ParseResult[T]
```

In addition to this simple version, Kiama overloads `parse` to take
input values of type `java.lang.CharSequence` and `java.io.Reader`.

It is often useful to make sure that a parser consumes all of its
input. The `phrase` combinator returns a parser that recognises what
its argument parser does, but only succeeds if there is no input
remaining.

```
def phrase[T] (p : Parser[T]) : Parser[T]
```

`phrase` is used by a set of `parseAll` functions that have the same
signatures as `parse` but require that the entire input be consumed
by a successful parse. Thus, a typical invocation of a parser `p` is

```
parseAll (p, s) match {
    case Success (e, _) =>
        println ("successful parse: " + e)
    case f              =>
        println (f)
}
```

Note that the printable representation of a `Failure` includes the
location of the failure, so it is almost always better to print the
entire `Failure` than just its message.

Up: [Parsing](Parsing.md), Next: ParserCombs
