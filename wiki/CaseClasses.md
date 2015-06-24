# Overview of Kiama's support for attribute grammars

Up: [Context](Context.md), Next: [Collections](Collections.md)

The easiest choice of representation for a data structure that is to
be manipulated by Kiama is one that comprises instances of Scala
[case classes](http://www.scala-lang.org/node/107)
(or case objects, which are singleton case classes).  These classes
can have fields that are instances of other cases classes, or are
of some non-structured type (integer, double, string, etc)

For example, the following declarations define a structure to
represent expressions in a programming language.

```
abstract class Exp
case class Num (d : Int) extends Exp
case class Var (s : String) extends Exp
case class Neg (e : Exp) extends Exp
case class Add (l : Exp, r : Exp) extends Exp
case class Sub (l : Exp, r : Exp) extends Exp
case class Mul (l : Exp, r : Exp) extends Exp
case class Div (l : Exp, r : Exp) extends Exp
```

and an instance of this structure can be created as follows

```
Add (Num (3), Var ("sum"))
```

representing the expression

```
3 + sum
```

## Repeated constructs

It is often useful to represent repeated constructs and optional ones.
Repeated constructs are encoded in a Kiama-compatible way using a
Scala sequence (of type `Seq`). For example, the following definitions
use this approach to represent statement sequences.

```
abstract class Stmt
case class Null () extends Stmt
case class Seqn (ss : Seq[Stmt]) extends Stmt
case class Asgn (s : Idn, e : Exp) extends Stmt
case class While (e : Exp, b : Stmt) extends Stmt
```

With these definitions, since a `List` is a kind of `Seq` constructed
using case classes, the following expression

```
Seqn (List (Asgn ("sum", Num (0)),
            Asgn ("i", Num (10)),
            While (Var ("i"),
                   List (Asgn ("sum", Add (Var ("sum"), Var ("i"))),
                         Asgn ("i", Sub (Var ("i"), Num (1)))))))
```

represents the program fragment

```
sum = 0;
i = 10;
while (i) {
    sum = sum + i;
    i = i - 1;
}
```

Note that Scala `List` values are implemented using the case class
`::` and the case object `Nil`. Thus, they conform to the assumption
here that all non-primitive values are constructed from case classes
or case objects.

(It is also possible to use a Scala
[iterated formal parameter](http://www.scala-lang.org/node/122) to
encode a repeated construct, but in our experience it's not worth it,
since iterated parameters can only appear at the end of a parameter
list and repeated constructs often occur in other positions. Also,
matching iterated parameters can be a bit more complicated. The price
to be paid for not using iterated parameters is that construction is
more complex since the sequence must be explicitly constructed, but
this usually only happens in a few places, so it's not much of a
burden.)

## Optional constructs

An optional construct should be encoded using a Scala `Option` type
(which is implemented using the case class `Some` and the case object
`None`). For example to represent class declarations in a Java-like
language, we might use

```
case class IdUse (Name : String)
case class ClassDecl (Name : String, Superclass : Option[IdUse],
                      Body : Block) extends TypeDecl
```

so that

```
ClassDecl ("Object", None, ...)
ClassDecl ("ColouredPoint", Some (IdUse ("Point")), ...)
```

represent

```
class Object { ... }
class ColouredPoint extends Point { ... }
```

## Other collections

List and option children in the tree structure are special cases of
collection nodes. Since they are implemented by case classes or objects
there is no special processing required.

Kiama also supports nodes that are collections that are not implemented
as case classes or objects. See [Collections](Collections.md) for more information.

## What can you do

Structures encoded using these approaches can be [rewritten](Rewriting.md)
or can have [attribute values](Attribution.md) defined for them. Rewriting
really only makes sense if the structure is a tree, whereas
attribution can also be applied to graph structures.

## Be wary of case objects and attribution

If you intend to perform attribution, you almost certainly don't want
to use case objects in your structure, except for things such as empty
lists or missing optional values. Since there is only one instance of
a case object, all of its occurrences in the structure will share
attributes, which is probably not what you want. For this reason, you
should probably avoid computing attributes directly of lists and
options, since if the value is `Nil` or `None` then the attributes
will be shared with all other empty lists or missing options.

The main alternative to computing attributes of case objects is to use
a case class instead. For example, in the statement definition above,
a case class is used for null statements, because each null statement
needs to have a different identity. If you want to have attributes of
a list or option value, wrap the list or option in a case class
instance and compute the attributes on that value instead.

## Generic structure access

On rare occasions you will want to access your structure in a generic
way. For example, you might want to write a method that takes an
expression and iterates over its children. Iterating over the children
of case class instances is automatically provided in Scala because the
compiler implements the `Product` interface for every case class.

However, an abstract class such as `Exp` or `Stmt` above will not
expose the `Product` interface. Therefore, if you want to process
expressions or statements in a generic way, you will want to declare
those classes as follows:

```
abstract class Exp extends Product
abstract class Stmt extends Product
```

You may want to obtain more generic support for your tree nodes. The
[Attributable](Attribution#Attributable.md) class and object in Kiama's
attribution module provides some useful facilities for this purpose.

## Positions

It is common to want to attach position (coordinate) information to
your structure to help with producing sensible messages. See
[ParserCombs#Positions](ParserCombs#Positions.md) for a discussion of how to do this when using
Scala parser combinators and [Messaging](Messaging.md) for information on how the
positions can be used.

Up: [Context](Context.md), Next: [Collections](Collections.md)
