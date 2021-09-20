# Parser basics, inputs and results

Up: [Parsing](Parsing), Next: [Parser combinators](ParserCombs)

See also [Parsing](Parsing) and [Parser combinators](ParserCombs) for general information about defining
and using parser combinators.

Most parsers for use with Kiama will operate on character input.
For simplest usage extend the following class:

```
org.bitbucket.inkytonik.kiama.parsing.Parsers
```

The `Parsers` trait gives you backtracking parsers that
_memoise_ their results, so that they allow unlimited lookahead
and performance that is linear in terms of the input size.  They
also allow left recursive productions to be directly encoded.
`Parsers` uses `Vector` values to record repeated constructs;
use `ListParsers` if you prefer to use `List` values for this
purpose.

The `Parsers` class uses a `Positions` data structure to keep
track of the input positions of parsed values. You should
provide one when you extend the class.

```
class SyntaxAnalysis (positions : Positions) extends Parsers(positions) {
    ... parser definitions ...
}
```

Thus, the usual approach is as follows where the `positions` argument to `SyntaxAnalysis` is provided by the code that creates the parser.

```
import org.bitbucket.inkytonik.kiama.util.Positions

val positions = new Positions
val parsers = new SyntaxAnalysis (positions)
```

## Parser sources and inputs

A parser processes sources of type `org.bitbucket.inkytonik.kiama.util.Source`.
The most common type of source is a `FileSource` that obtains its input from a file.

```
import org.bitbucket.inkytonik.kiama.util.FileSource

val source = FileSource ("file.txt")
```

The class `StringSource` is also available so that a string can be used instead of a file.

Parser input then is a source combined with an offset which records the current parsing positions.

```
case class Input (source : Source, offset : Int)
```

## Parser results

A parser that returns a value of type `T` on success is of type `Parser[T]`
which extends the function type `Input => ParseResult[T]`.
`ParseResult` has sub-classes to represent the possible outcomes of
a parse.

```
case class Success[T] (result : T, next : Input) extends ParseResult[T]
abstract class NoSuccess(message : String, next: Input) extends ParseResult[Nothing]
case class Failure (message : String, next : Input) extends NoSuccess (message, next)
case class Error (message : String, next : Input) extends NoSuccess (message, next)
```

In each case, the `next` parameter represents the input remaining after
the parse. `Success.result` is the value produced by the successful
parse, whereas a `message` field is the message resulting from a failed
parse.  A `Failure` represents a failure that can backtrack to retry
earlier parses, whereas an `Error` represents a final error.

## Running a parser

Since a parser is just a function, it can be applied directly to an
input to get a result. I.e., `p (in)` for a parser `p` and input `in`.

Invocation of a parser is also encapsulated by the `parse` function,
provided by `Parsers` which begins parser at the start of a source:

```
def parse[T] (p : Parser[T], source : Source) : ParseResult[T]
```

It is often useful to make sure that a parser consumes all of its
input. The `phrase` combinator returns a parser that recognises what
its argument parser does, but only succeeds if there is no input
remaining.

```
def phrase[T] (p : Parser[T]) : Parser[T]
```

`phrase` is used by `parseAll` that has the same
signature as `parse` but requires that the entire input be consumed
by a successful parse. Thus, a typical invocation of a parser `p` is

```
parseAll (p, s) match {
    case Success (e, _) =>
        println ("successful parse: " + e)
    case f =>
        println (f)
}
```

In the `Success` case `e` is the value created by the parse.

Note that the printable representation of a `Failure` includes the
location of the failure, so it is almost always better to print the
entire `Failure` than just its message.

Up: [Parsing](Parsing), Next: [Parser combinators](ParserCombs)
