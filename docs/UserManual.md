# Kiama Language Processing Library

Up: [Documentation](Documentation), Next: [Examples](Examples)

Kiama is a [Scala](http://www.scala-lang.org) library for language
processing. "Language" in this context refers to any structured
information, usually the text of programming languages, data formats
or natural languages, but it could come from any source.

Kiama can be used to write a variety of language processors. Typical
applications include:

  * compilers and interpreters for programming languages,
  * checkers and analysers of language properties,
  * language formatters and pretty-printers,
  * transformations of a single language (e.g., for optimisation),
  * translations between languages, and
  * implementations of abstract machines.

A number of high-level processing paradigms have been developed in the
research field of language processing. The aim of the Kiama project is
to make these paradigms available to Scala programmers in a way that
integrates smoothly with normal Scala code. Standard Scala tools such
as compilers and integrated development environments can be used to
develop Kiama programs. The barriers to using the supported processing
paradigms via Kiama are therefore lower than approaches that require
separate tools.

## The User Manual

This user manual provides an overview of Kiama's capabilities.
Familiarity with Scala is assumed. For more information on Scala and
implementation downloads, see the
[Scala web site](http://www.scala-lang.org).
The book
[Programming in Scala](http://www.artima.com/shop/programming_in_scala)
by Odersky, Spoon and Venners is highly recommended.

The rest of this user manual contains the following sections:

  * [an overview of the context in which the Kiama library facilities are used](Context)
  * Some discussion of how to get data into a form that Kiama can manipulate:
    * [Parsing](Parsing)
  * one section for each of the major processing paradigms supported by Kiama:
    * [strategy-based rewriting](Rewriting)
    * [attribute definition](Attribution)
    * [tree relations](Relations)
    * [abstract state machines](Machines)
    * [pretty printing](PrettyPrinting)

[Examples](Examples) are also available, ranging from simple illustrations of
usage to more complex language implementations and processing tasks.
New users should read the user manual and then consult the examples
for more detail.

More research-oriented material in the form of papers, talks and slides can be
found on the [Research](Research) page.

API documentation can be found by searching for Kiama on [javadoc.io](https://www.javadoc.io).
Alternatively, links to the API documentation for specific releases can be found on the [Releases](Releases) page.

Up: [Documentation](Documentation), Next: [Examples](Examples)
