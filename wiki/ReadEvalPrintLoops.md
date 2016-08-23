# How to write read-eval-print loops using Kiama

Up: [User Manual](UserManual.md), Prev: [Messaging](Messaging.md), Next: [Profiling](Profiling.md)

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

Some language processors operate most naturally as interactive
software that reads user input, turns that input into a structured
representation (usually an abstract syntax tree), processes it and
then prints some result. In the programming languages field, this kind
of interaction is called a _read-eval-print loop_ (or _REPL_).

Kiama provides simple support in the `org.bitbucket.inkytonik.kiama.util` package for creating
REPLs of various kinds. The following examples make particular use of
the REPL support:

  * [Imperative](Imperative.md)
  * [Lambda](Lambda.md)
  * [Lambda2](Lambda2.md)

## The `REPL` trait

File: [org.bitbucket.inkytonik.kiama.util.REPL.scala](http://code.google.com/p/kiama/source/browse/library/src/main/scala/org/bitbucket/inkytonik/kiama/util/REPL.scala)

The common interface for REPLs built using Kiama is provided by the
`REPL` trait. It provides a `main` function that:

  * invokes a `setup` method
  * creates a new console reader
  * enters an infinite loop that repeatedly:
    * prints a prompt and reads a line of input from the console
    * if the line is null (indicating end of file), returns from `main`
    * otherwise, calls `processline` passing the line as an argument

Methods like `setup` must be provided by an implementation of the `REPL`
trait.  The complete list of things that must be provided is:

  * `def setup : Unit`

> Perform any setup required before the REPL starts.

  * `def prompt : String`

> Return the prompt that should be printed before an input line is read.

  * `def processline (line : String) : Unit`

> Process the given line.

`REPL` uses the `JLine` library to handle prompting and to provide line
editing and history.

## `ParsingREPL`

File: [org.bitbucket.inkytonik.kiama.util.REPL.scala](http://code.google.com/p/kiama/source/browse/library/src/main/scala/org/bitbucket/inkytonik/kiama/util/REPL.scala)

`REPL` provides a very general interface. Kiama also provides a number
of more specialised instantiations of `REPL` for common situations.

`ParsingREPL[T]` provides a `processline` function that parses the
input line using a provided parser and, if the parse is successful,
passes the parsed representation to another method for processing. If
the parse is unsuccessful an error message will be printed. The type
parameter `T` is the type of the parsed representation. Instead of
providing a `processline` implementation you should provide:

  * `def start : Parser[T]`

> A [parser](Parsing.md) that returns a value of type T.

  * `def process (t : T) : Unit`

> A method to process the `T` values produced by the parser.

## `GeneratingREPL`

File: [org.bitbucket.inkytonik.kiama.util.REPLTests.scala](http://code.google.com/p/kiama/source/browse/library/src/test/scala/org/bitbucket/inkytonik/kiama/util/REPLTests.scala)

A less useful variant of `REPL` is `GeneratingREPL[T]` which provides
a standard interface for REPLs that randomly generate program
instances. This kind of REPL is typically used for testing so that you
can check that your [ScalaCheck](http://code.google.com/p/scalacheck/)-based
random generation code is producing reasonable programs (for some
definition of _reasonable_).

`GeneratingREPL` defines versions of `setup` and `prompt` that print
sensible messages, although you can override them if you want. It also
defines a `processline` that uses a provided generator to produce an
instance of type `T` and then passes it on for processing.  You should
provide:

  * `def generator : Arbitrary[T]`

> Generate a random value  of type `T`.  See the
> [ScalaCheck](http://code.google.com/p/scalacheck/) documentation for
> how to write this method.

  * `def process (t : T) : Unit`

> Process the randomly-generated `T` value.  Default: print it.

## JLine versions

Kiama implements REPLs using the
[JLine 2 library](https://github.com/jline/jline2).
In some cases, this version of the JLine library clashes with the version
used by the simple build tool (sbt). In those cases you will get an
incompatible class exception when you try to run a REPL.
If this happens, a work-around is to configure sbt to fork runs so that
they run in a different JVM to the clashing library.
The following sbt configuration lines are sufficient to make this
happen.

```
fork in run := true

connectInput in run := true

outputStrategy in run := Some (StdoutOutput)
```

Up: [User Manual](UserManual.md), Prev: [Messaging](Messaging.md)
