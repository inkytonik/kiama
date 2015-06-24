# Overview of Kiama's support for attribute grammars

Up: [User Manual](UserManual.md), Prev: [Rewriting](Rewriting.md), Next: [Machines](Machines.md)

This page provides an overview of Kiama's support for
_attribute grammars_. For the context in which this part of the library
operates, see [Context](Context.md). Attribute grammars are used in the following
examples.

  * [Dataflow](Dataflow.md)
  * ISWIM
  * [Lambda2](Lambda2.md)
  * [Oberon0](Oberon0.md)
  * Obr
  * OneOhOneCompanies
  * PicoJava
  * Prolog
  * Repmin
  * Transform

More information about Kiama attribute grammars can be found via the
[Research](Research.md) page.

## Introduction

Language constructs frequently have properties that a language
processor must compute. Examples include types for expressions and
variables in programming language compilers, values of expressions in
interpreters, or metrics for classes or methods in software analysis
tools. [Attribute grammars](http://en.wikipedia.org/wiki/Attribute_grammar)
are a well known technique for specifying how to compute properties,
called _attributes_, of constructs by defining their values in terms of
the attribute values of related constructs. Traditional
attribute grammar systems take as input the equations that define the
attributes and generate a program that can evaluate the attributes for
a tree that represents a particular program.

Kiama supports attribute grammars in the form of first-class attribute
values that compute properties of Scala data structures. In many cases
the data structures are trees but graphs are also supported. The style
of attribute grammars supported by Kiama is closely related to those
of the [JastAdd](http://jastadd.org) system in that attributes
are evaluated on demand and their values can be cached to avoid
recomputation.

Kiama attributes are defined as functions by normal Scala code rather
than being generated as method definitions from a special-purpose
attribute grammar language as in the JastAdd system. The Kiama
approach enables attributes to be defined on a pre-existing class
hierarchy.

The Kiama attribute grammar library is made available by the object
`org.kiama.attribution.Attribution`.
Import the types and methods you need from that object. (There is
also an `Attribution` trait so you can mix it in with your own
functionality.)

For example,

```
import org.kiama.attribution.Attribution._
```

imports all names from the `Attribution` object.

To get full support for pattern matching in
attribute definitions it is best to use data structures constructed
from instances of case classes as described in [Context](Context.md).

## Repmin

To illustrate the basic idea of attribute grammars in Kiama, consider
the following well-known simple problem, usually called _Repmin_. The
aim is to process a binary tree containing integer values at the
leaves. The output for a given tree should be a new tree with the same
structure as the input tree but with each leaf replaced by a leaf that
contains the minimum leaf value from the entire input tree. Thus, some
analysis of the input tree is required as well as production of a new
tree.

Suppose that the tree structure for Repmin is defined as follows.

```
abstract class Tree
case class Fork (left : Tree, right : Tree) extends Tree
case class Leaf (value : Int) extends Tree
```

If the input tree is

```
Fork (Leaf (3), Fork (Leaf (1), Leaf (10)))
```

we want to see the following tree as output.

```
Fork (Leaf (1), Fork (Leaf (1), Leaf (1)))
```

## Attributes

If you were not using attribute grammars, you would probably program
some traversals of the tree, perhaps using the Visitor design pattern.
One traversal might collect the minimum leaf value and another might
build the output tree using that value. In an attribute grammar
approach we just define the attributes of different nodes in the tree
in terms of attributes of adjacent nodes. No explicit traversal is
encoded; it is implicit in the dependencies between the attributes.

A common method for designing an attribute grammar is to reason
backwards from the information that we want. In this case, we want a
tree that contains leaves that are the minimum value of the original
tree. This suggests that the output should be an attribute of the root
of the input tree whose value is the output tree. In Kiama this
attribute signature would look like this:

```
val repmin : Tree => Tree
```

In the type of `repmin` the `=>` indicates that `repmin` is a function,
in this case from type `Tree` to type `Tree`. We can use `repmin` as
an ordinary function without having to worry about its extra _attribute_
behaviour, such as caching its results.

To define `repmin` we will need to know the minimum leaf value of the
input tree. Let's define another attribute for that.

```
val globmin : Tree => Int
```

Finally, to define the `globmin` attribute we will need to recursively
compute the mimima in each sub-tree of the input tree. We use a
`locmin` attribute for those values.

```
val locmin : Tree => Int
```

## Defining attributes

Having determined which attributes we want, we now proceed to define
them. To define `repmin` we assume that `globmin` is already
available. The `repmin` of a leaf node is just a new leaf containing
the global minimum. The `repmin` of a pair node is just a new pair
containing the `repmin` trees of the pair's children. In Kiama these
equations can be stated as follows.

```
val repmin : Tree => Tree =
    attr {
        case Fork (l, r) => Fork (l->repmin, r->repmin)
        case t : Leaf    => Leaf (t->globmin)
    }
```

Standard Scala pattern matching suffices to distinguish between the
two different cases. The `attr` function creates the attribute value
based on the equations. `attr` can be regarded as packaging the
equations up as a function and wrapping them in caching behaviour so
that an attribute of a node is evaluated at most once.

The attribute creation functions such as `attr` also have variants
where you can specify a name for the attribute. The name is used
when the attribute is printed, such as when the library reports a
cycle in the attribute's dependencies. Use the form

```
attr ("name") { ... }
```

to specify a name.

The `n->a` notation is used to refer to attribute `a` of node `n`.
Since attributes are just functions, `n->a` is exactly the same as `a (n)`.
The arrow form is usually used since it puts the focus on the
node, but the function call form can be useful sometimes to
match domain notation more closely. (For example, see the `in` and `out`
attributes in the [Dataflow](Dataflow.md) example.)

Continuing with the other Repmin attributes, we can define the local
minimum as follows.

```
val locmin : Tree => Int =
    attr {
        case Fork (l, r) => (l->locmin) min (r->locmin)
        case Leaf (v)    => v
    }
```

In this definition the local minimum at a leaf node is the value at
that leaf and the minimum at a pair is the minimum of the minima of
the two sub-trees.

Finally, the global minimum can be defined as follows.

```
val globmin : Tree => Int =
    attr {
        case t if t isRoot => t->locmin
        case t             => t.parent[Tree]->globmin
    }
```

## Attributable

This definition makes use of some meta-level properties of the tree
that Kiama provides automatically if the tree nodes inherit from
`org.kiama.attribution.Attributable`.  For Repmin, the following suffices.

```
abstract class Tree extends Attributable
```

`Attributable` provides access to information about the tree such as
the parent of a given node (`parent`) or whether the node is the root
of the tree (`isRoot`). These properties are made available by the
`Attributable` class.

You must call the method `Attribution.initTree(t)` on each tree
root `t` if you want to access these node properties within the
tree rooted at `t`. `t` must extend `Attributable`. It is not
necessary to call it on every node in the tree, just on the root.
We suggest that you perform this initialisation once after the tree
has been constructed but before you evaluate any attributes on it.
You must also initialise any trees that you construct during
attribution if you want to evaluate attributes on them later.

(Versions of Kiama before 1.2.0 initialised node properties in the
`Attributable` constructor. This approach was not robust since it
was hard to keep track of when nodes were constructed in relation
to their parents and children. Thus, in 1.2.0 the `initTree` method
was introduced to make the initialisation more explicit and
predictable.)

`Attributable` also provides other properties that are derived from
`parent`, such as `children` that contains all of the children nodes
of a node, and `prev` and `next` that provide access to nodes on the
same level as a node. All of these node properties will work for any
child that implements `Attributable`, but also recursively for the contents
of any tuple, `Some` or `GenTraversable` child. For example, if `x`
is a node that has a tuple child which contains `Attribtuable` node
`y`, then the parent of `y` will be `x`. See [Collections](Collections.md) for more
discussion of this point.

We use both `parent` and `isRoot` in the definition of `globmin`. If
we are asking for the global minimum of the root of the tree, then
it's just the local minimum of the sub-tree rooted at that node. Other
nodes just ask their parent for their global minimum value. Thus the
request propagates up the tree to the root and the value propagates
down.

In the second case for `globmin` we use the form `t.parent[Tree]` to
refer to the current node's parent. The type information `Tree` is
necessary in this access since the `Attributable` class does not know
the structure of the tree. Thus `parent` has a generic type to which
`globmin` cannot be applied. An alternative would be to define
`globmin` to have type

```
val globmin : Attributable => Int
```

but this would imply some loss of checking that `globmin` can only be
applied to trees. This tradeoff is one of the sacrifices that
currently has to be made to define attribute grammars as standard
Scala programs without any pre-processing or meta-programming.

`Attributable` provides some other operations such as the `->`
notation for accessing attributes and a `clone` method. The
`Attributable` companion object provides a `deepclone` operation
that uses the Kiama [Rewriting](Rewriting.md) library and `clone` to clone a
whole `Attributable` tree.

(Kiama also provides a
[Patterns](http://wiki.kiama.googlecode.com/hg/doc/1.4.0/api/index.html#org.kiama.util.Patterns$)
utility module that provides extractor objects which are useful for
dealing with node properties. For example, `HasParent` can be used
to match on both a node and its parent at the same time.)

If you don't want to use the things that `Attributable` provides, you
don't need to use it.  In that case, you will need to provide node
properties in some other way, if needed.  Also, you will need to use
the `a(n)` form of attribute reference since `->` will not be
available.

## Embedding attribute grammars

Repmin illustrates the main ideas of attribute grammars and their
incarnation in Kiama. Attributes of tree nodes are defined in terms of
either basic values or of attributes of adjacent nodes. An evaluation
mechanism behind the scenes takes care of evaluating the attributes
when they are needed and remembering their values in case they are
needed again.

In most attribute grammar systems the equations are processed by a
generator to produce an attribute evaluator. In Kiama the equations
are themselves a program that can be compiled and executed without any
special processing. As a result, attributes can easily be used from
other Scala code, just by regarding the attribute as a function.

The rest of this page summarises the Kiama attribute library,
particularly focussing on features not used in the Repmin example.

## Node properties

In addition to the `parent` and `isRoot` node properties described
earlier, Kiama also provides properties to provide information about
nodes that occur in sequences.  Suppose that your tree structure
contains a node type like this:

```
abstract class Stmt extends Attributable
case class Seqn (ss : Seq[Stmt]) extends Stmt
```

where sequences of statements are also statements. The Attributable
class provides nodes in the sequence with properties to enable them to
query their surroundings. If `s` is a node in the sequence, then
`s.next` refers to the next node in the sequence (or null if there is
none) and `s.prev` refers to the previous node in the sequence (or
null if there is none). The Boolean-valued properties `isFirst` and
`isLast` can be used to test if a sequence member occurs first or last
in the sequence, respectively.

(In fact, these attributes are more general than shown here. They also
work in contexts where the node in question has other `Attributable`
siblings (i.e., at the same tree level) but not within the same
sequence.)

The `parent` node property and the sequence member properties only pay
attention to the `Attributable` nodes in the structure. For example,
the parent of a node in the `ss` component of a `Seqn` construct will
be the `Seqn` node, not the `Seq[Stmt]` value. This behaviour is
designed to allow attribution to be conveniently attached to the relevant
semantic nodes rather than nodes that only serve to implement the
structure. For example, in the example above, the `Seqn` node
represents the sequence so it is the one that should have attributes
of the sequence. Similar remarks apply to nodes that occur inside
`Option` and `Either` values.  In fact, `Seq` values are a special
case; in general, any `Traversable` value is treated as described.

## Attribute definitions

Many attributes can be defined using the `attr` function as seen in
the Repmin example. As noted earlier, attributes cache their results
so that if they are called more than once on a particular node, the
attribute computation is only performed the first time. You can clear
the cache using an attribute's `reset` method, but this is rarely
needed. All attribute caches can be lazily reset using the method
`Attribution.resetMemo`. Cached attributes also have a method
`hasBeenComputedAt` which can be used to determine whether an
attribute has already been computed at a particular node.

In some cases, it is useful to be able to examine the parent context
when defining an attribute. It is possible to explicitly match on the
`parent` property of a node in an `attr` definition, but Kiama also
provides the `childAttr` function to make this a bit easier. A
definition of the form

```
val a : CachedAttribute[U,V] =
    childAttr {
        upat => {
            ppat => {
                case ... => ...
                ...
            }
            ...
        }
        ...
    }
}
```

matches both on the `U` node to which it is applied (with `upat`) and
on the parent of the `U` node (with `ppat`). An example of the use of
`childAttr` can be found in the [Dataflow](Dataflow.md) example.

## Reference attributes

Attribute grammar systems such as JastAdd provide so-called _reference
attributes_ whose values are references to nodes in the tree that is
being attributed.  Reference attributes are useful when properties of
some nodes in the tree are easily obtained at other nodes in the tree.

For example, in a programming language processor we might have
declarations of identifiers as well as uses of those identifiers. In a
process called _name analysis_ the processor will want to associate
each use with its corresponding declaration, reporting an error if no
such declaration can be found. A reference attribute of an identifier
use can be used to point directly to the associated declaration node.

Reference attributes can be defined in Kiama just like other attribute
values. Nothing special needs to be done to represent the reference;
it's just a normal Scala reference. See the [Dataflow](Dataflow.md) example for
attributes that are defined by reference to represent the control flow
of a program.

Reference attributes may also refer to nodes in newly-constructed
pieces of tree. This might be useful if the attribution is translating
a construct, for example, where the new tree represents the
translation. The new tree can have attributes in just the same way as
the nodes in the main tree.

At the moment, the root of a tree-value attribute is always unrelated
to the main tree. In a future version of Kiama there will be a
mechanism to attach a calculated tree to another node, so that parent
references will be propagated to that node. We also plan to support
_forwarded attributes_ where attributes of a tree-valued attribute can
be automatically referred to another node.

## Parameterised attributes

A common way to define a reference attribute is by a search process
within the tree. For example, to look up an identifier use we might
define a `lookup` attribute that takes the identifier to be looked up
and returns a reference to the declaration node, or null if one is not
found. We say that `lookup` is a _parameterised attribute_ since its
definition depends on the identifier that is being looked up.

Kiama provides a `paramAttr` function to assist with defining
parameterised attributes.  The general form is

```
val a : CachedParamAttribute[T,U,V] =
    paramAttr {
        t => {
            ppat => {
                case ... => ...
            }
           ...
        }
}
```

where `T` is the type of the parameter, `U` is the type of tree nodes
for which this attribute is defined and `V` is the type of the
attribute value. See the `lookup` attribute of the [PicoJava](PicoJava.md) example
for a full example of the use of`paramAttr`.

`paramAttr` provides caching behaviour for parameterised attributes
where the attributes are cached using both the parameter value and the
tree node as the key. Instead of using `paramAttr`, it is also
possible to define parameterised attributes as normal methods that
return attributes as shown next. The difference is that the parameter
will not be taken into account when caching, plus the parameter and
the node are provided in separate parameter lists.

```
def a (t : T) : U => V =
    attr {
        ..
    }
```

## Circular attributes

Attributes as defined so far cannot depend on themselves. If the value
of an attribute is needed during its own evaluation, that evaluation
will be terminated with an error reporting a cycle in the attribute
dependencies.

In some cases cycles in the dependencies are natural because the
attributes being defined need to be evaluated repeatedly until a fixed
point is reached. We call attributes of this kind _circular
attributes_ following the terminology of systems such as JastAdd.
Kiama provides the `circular` function to enable circular attributes
to be defined. The general form is

```
val a : U => V =
    circular (v) {
        case ... => ...
        ...
    }
```

In this definition `v` (of type `V`) is the initial value of the
attribute and the cases are used to define subsequent values in terms
of other, possibly circular, attributes.

See the [Dataflow](Dataflow.md) example for an illustration of the use of circular
attributes to define iterative dataflow equations to compute live
variables for an imperative programming language.

## Tree splicing

Encodings of translations using attribute grammars commonly use `tree
splicing` to connect an original tree (representing the thing that is
being translated) to a new tree or trees (representing the
translation).

As we have seen above with the `globmin` attribute, it is easy to have
attributes whose values are trees. Tree splicing adds one more twist:
the root of the tree that is spliced-in gains a parent, rather than
being parentless as in the `globmin` case. This new parent is useful
if computations on the spliced tree need to be able to access their
context (represented by the parent and its ancestors).

See the Transform example for an illustration of this technique to
support user-defined operator priorities in an expression language.

Tree splicing is easily achieved by defining an attribute using the
`tree` attribute function. `tree` operates just like `attr` except
that it also performs the splicing operation. Here is the relevant
snippet from the Transform example.

```
val ast : ExpR => Exp =
    tree {
       case e => e->op_tree
    }
```

`ExpR` is the type of expression nodes in the original tree. Each
`ExpR` node has an `op_tree` attribute which defines a translation of
that node, just as `globmin` defines a translation of a Repmin node.
The `ast` attribute causes the `op_tree` value to be spliced in so
that the new tree has the same parent as the node in the old tree. In
other words, `n->ast` will have the same value as `n->op_tree` but
will also have the same parent as `n`.

Using a tree splice, we can explicitly access attributes of new trees,
via the nodes in the old tree. For example, if we want to access the
`errors` attribute of the translation of node `n`, we can use
`n->ast->errors`. The computation of the `errors` attribute has access
to the context of node `n` since the `ast` attribute is spliced in.

## Attribute forwarding

It is sometimes desirable to refer to attributes such as `errors`
implicitly instead of specifying the exact access path via a spliced
tree. For example, we might like to refer to `n->errors` and have that
_forwarded_ automatically to the `ast` attribute. This idea allows us
to decouple the use of the attribute `errors` from the precise way in
which its value is computed (by passing it off to the `ast`
attribute). Thus, the details of the latter can vary without requiring
all uses of `errors` to be updated.

In our example, we can add forwarding by simply making the definition
of `ast` an implicit Scala definition. The right-hand side of the
definition is the same as before.

```
implicit val ast : ExpR => Exp = ...
```

Thus, if we try to evaluate `n->errors`, where `n` is an `ExpR`, but
`errors` is defined only on `Exp` nodes, the implicit conversion will
kick in to convert `n` to an `Exp`.

## Decorators

The module `org.kiama.attribution.Decorators` provides _decorators_
that implement short-hand notation for common patterns of attribution.

The simplest decorator provided by this module is `down`. The idea
is to capture the pattern of attribution where an attribute is only
defined at some nodes in the tree, perhaps just at the root. At nodes
where the attribute is not defined, we want to use the value of the
attribute at the closest ancestor node where it is defined. In other
words, we want to copy the attribute value from that closest ancestor
down to the node where we need it. Since this pattern is general, the
`down` decorator captures it so it can be reused.

`down` has the following signature:

```
def down[T <: Attributable,U] (a : T ==> U) : T => U
```

The attribute that results from the `down` decorator can only be
used on `Attributable` nodes since it uses the `parent` property
to find the closest ancestor. The parameter `a` is a partial
function that defines the behaviour of the attribute at those
nodes where it is defined directly. Wherever `a` is not defined
the decorator will ask the parent for the value. For this reason,
`a` should at least be defined at the root of the tree.

`T ==> U` is an alias for the Scala library type `PartialFunction[T,U]`.
You can obtain the `==>` type constructor for use in your own code
by importing it from the `org.kiama` package:

```
import org.kiama.==>
```

The other decorator provided by the library at the moment is `chain`.
It enables you to thread a value through a tree, going past every
node. As the value goes past a node, either downwards on its way to
the node's children or upwards to the node's parent, functions that
are supplied to the decorator can be used to update its value in a
problem-specific way.

To see the `chain` decorator in action, see the name analysis portion
of the Oberon0 example. In that example, a chain is used to thread
an environment of bindings through the tree that represents a
program. The chain is updated at nodes that represent identifier
definitions to add those definitions to the environment. The chain
is also updated at blocks to define nested scopes. Finally, it is
accessed at identifier use nodes in order to check those uses against
the environment at that point.

Up: [User Manual](UserManual.md), Prev: [Rewriting](Rewriting.md), Next: [Machines](Machines.md)
