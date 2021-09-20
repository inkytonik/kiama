# Converting text into structures that Kiama can process

Up: [User Manual](UserManual), Prev: [Context](Context), Next: [Rewriting](Rewriting)

It is necessary to have a tree representation of the data that is
to be manipulated in order to use Kiama's facilities. This
representation is usually an _abstract syntax tree_ (AST) that
encodes the structure of a program written in some language, but
the tree could represent any hierarchical data.

There are currently two convenient methods for building tree
structured representations for use with Kiama: using the Scala parser
combinator library and using the separate
[Rats! parser generator](http://cs.nyu.edu/rgrimm/xtc/rats.html). For
documentation on how to use Rats! with Scala, please see the
[sbt-rats](https://github.com/inkytonik/sbt-rats) project and the Rats!
documentation. For simplicity, the remainder of this documentation
focuses on combinator-based parsing.

_Combinator-based parsing_ is common in languages such as Haskell and ML
in packages such as
[Parsec](http://book.realworldhaskell.org/read/using-parsec.html). The
basic idea of combinator parsing is to write expressions that look
like the context-free grammar productions of the language that is to be parsed. The
value of an expression is a parsing object that can be
applied to a specific input source. The result of that application is
either an indication of parsing success and possibly a value
representing the parsed text, or an indication of failure and a
failure message.

Detailed documentation for the parsing library may be found in the
[Kiama API documentation](https://www.javadoc.io/doc/org.bitbucket.inkytonik.kiama/kiama_2.12/2.2.0) (search "Parsers" to start).
Chapter 31 of
[Programming in Scala](http://www.artima.com/shop/programming_in_scala)
provides an excellent overview of a similar library that used to be part of the Scala standard library.
This chapter from the first edition of the book is freely [available](http://www.artima.com/pins1ed/combinator-parsing.html).

More detailed information on using the parsing library with Kiama can be
found in these pages:

  * [Parser basics, inputs and results](ParserInput)
  * [Constructing parsers](ParserCombs)

Up: [User Manual](UserManual), Prev: [Context](Context), Next: [Rewriting](Rewriting)
