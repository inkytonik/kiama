# Description of the context in which Kiama operates

Up: [User Manual](UserManual), Next: [Parsing](Parsing)

The Kiama library operates on structured information, often
representing text. An in-memory form of the information is the central
data structure of Kiama-based processors. In most cases, the
representation is a tree, but graphs are also possible.

Kiama makes no assumptions about the origin of the data structure that
it processes. In many projects, the data structure will be created by
a parser during analysis of a textual form. (Scala includes a
[parsing library](Parsing) to help with this task.) Alternatively, the
data can be obtained from another source such as a serialised representation
or a database, or it can be created programmatically.

There are a number of different options for the types used to represent
a structure that is to be manipulated by Kiama.  The most appropriate
choice depends on where the data comes from and how much control you
have over its type.  The current options are:

  * If you are creating the data structure within Scala and have no other constraints on its type, the best option is to use [case classes](http://www.scala-lang.org/node/107). All of the Kiama facilities work on case class instances without any extra definitions or code being required. [More information](CaseClasses).

  * If your data contains instances of Scala [collection classes](http://www.scala-lang.org/docu/files/collections-api/collections.html) such as sets or maps, then you are also well-placed.  Kiama supports processing of most sequential collection types. [More information](Collections).

  * Types that are not otherwise covered, but that you can modify.  [More information](Rewritable).

Of course, these options are not mutually exclusive. A data type that
combines types from different items in the list above can be used with
Kiama. You will need to support each component of that data type as
appropriate.

For example, a common case is when you use a mixture of the first two
of the options: case class types, where some of the fields are of
collection type. Nothing special needs to be done since all of
components are supported by Kiama without any extra work.

As another example, if you have a collection that contains values of
some non-case class Scala type, you will need to provide special support for
the non-collection parts.

Up: [User Manual](UserManual), Next: [Parsing](Parsing)
