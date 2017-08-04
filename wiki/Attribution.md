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

The Kiama attribute grammar library is made available by the class
`org.bitbucket.inkytonik.kiama.attribution.Attribution`.
Extend that class to build a module that uses attribution.

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
sealed abstract class RepminTree extends Product
case class Fork(left : RepminTree, right : RepminTree) extends RepminTree
case class Leaf(value : Int) extends RepminTree
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
val repmin : RepminTree => RepminTree
```

In the type of `repmin` the `=>` indicates that `repmin` is a function,
in this case from type `RepminTree` to type `RepminTree`. We can use `repmin` as
an ordinary function without having to worry about its extra _attribute_
behaviour, such as caching its results.

To define `repmin` we will need to know the minimum leaf value of the
input tree. Let's define another attribute for that.

```
val globmin : RepminTree => Int
```

Finally, to define the `globmin` attribute we will need to recursively
compute the mimima in each sub-tree of the input tree. We use a
`locmin` attribute for those values.

```
val locmin : RepminTree => Int
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
        case Fork(l, r) => Fork(repmin(l), repmin(r))
        case t : Leaf   => Leaf(globmin(t))
    }
```

Standard Scala pattern matching suffices to distinguish between the
two different cases. The `attr` function creates the attribute value
based on the equations. `attr` can be regarded as packaging the
equations up as a function and wrapping them in caching behaviour so
that an attribute of a node is evaluated at most once.

Continuing with the other Repmin attributes, we can define the local
minimum as follows.

```
val locmin : Tree => Int =
    attr {
        case Fork(l, r) => locmin(l).min(locmin(r))
        case Leaf(v)    => v
    }
```

In this definition the local minimum at a leaf node is the value at
that leaf and the minimum at a pair is the minimum of the minima of
the two sub-trees.

Finally, the global minimum can be defined as follows.

```
val globmin : Tree => Int =
    attr {
        case tree.parent(p) =>
            globmin(p)
        case t =>
            locmin(t)
    }
```

## Tree relations

The definition of `globmin` uses a value `tree` that encapsulates
relational information about the abstract syntax tree.
We can use it to examine the tree structure to help us decide
which equation to use.
For example, in `globmin` we match `tree.parent(p)` which uses
the `parent` relation to see if the node we are at has a parent.
If it does have a parent, the match will succeed and `p` will
be bound to that parent.
Thus, the first case of `globmin` asks the parent for its
`globmin`.
The second case will only be used if the first fails, in other words,
if the node has no parent.
In that case, we just use `locmin` since a node with no parent is
the root of the tree and its `globmin` is the same as its `locmin`.

The only question remaining is "where does `tree` come from"?
It is usually created by the module that builds the attribution
module and passed as a constructor argument.

```
import org.bitbucket.inkytonik.kiama.relation.Tree

class Repmin(tree : Tree[RepminTree, RepminTree]) extends Attribution {
    ... attributes go here ...
}
```

The type `Tree` takes two type parameters: one to specify the type
of the tree nodes, and another to specify the type of the root of the
tree.
In this example they are both `RepminTree`.

If `ast` is the root of the actual abstract syntax tree, then the
following code is typical to create a `Tree` for that AST, make
an attribute module that uses it, and then use the attributes.

```
val tree = new Tree[RepminTree,RepminTree](ast)
val repmin = new Repmin(tree)
... use repmin.globmin etc ...
```

Kiama's tree relation support includes other relations such as
`prev` and `next` for the previous and next nodes at the same
level, or `firstChild` to get the first child of a node.
See the documentation of Kiama's `Tree` class for more details.

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

## Attribute definitions

Many attributes can be defined using the `attr` function as seen in
the Repmin example. As noted earlier, attributes cache their results
so that if they are called more than once on a particular node, the
attribute computation is only performed the first time. You can clear
the cache using an attribute's `reset` method, but this is rarely
needed. Cached attributes also have a method
`hasBeenComputedAt` which can be used to determine whether an
attribute has already been computed at a particular node.

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

## Parameterised attributes

A common way to define a reference attribute is by a search process
within the tree. For example, to look up an identifier use we might
define a `lookup` attribute that takes the identifier to be looked up
and returns a reference to the declaration node, or null if one is not
found. We say that `lookup` is a _parameterised attribute_ since its
definition depends on the identifier that is being looked up.

Kiama provides a `paramAttr` function to assist with defining
parameterised attributes.  
For example, the `lookup` attribute in the Kiama PicoJava example
is defined as follows so that it is parameterised by a string
name.

```
val lookup : String => PicoJavaNode => Decl =
    paramAttr {
        name =>
            {
                case ... =>
                case ... =>
            }
    }
```

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

## Tree splicing (Kiama 1.x only)

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
       case e => op_tree(e)
    }
```

`ExpR` is the type of expression nodes in the original tree. Each
`ExpR` node has an `op_tree` attribute which defines a translation of
that node, just as `globmin` defines a translation of a Repmin node.
The `ast` attribute causes the `op_tree` value to be spliced in so
that the new tree has the same parent as the node in the old tree. In
other words, `ast(n)` will have the same value as `op_tree(n)` but
will also have the same parent as `n`.

Using a tree splice, we can explicitly access attributes of new trees,
via the nodes in the old tree. For example, if we want to access the
`errors` attribute of the translation of node `n`, we can use
`errors(ast(n))`. The computation of the `errors` attribute has access
to the context of node `n` since the `ast` attribute is spliced in.

## Attribute forwarding

It is sometimes desirable to refer to attributes such as `errors`
implicitly instead of specifying the exact access path via a spliced
tree. For example, we might like to refer to `errors(n)` and have that
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

Thus, if we try to evaluate `errors(n)`, where `n` is an `ExpR`, but
`errors` is defined only on `Exp` nodes, the implicit conversion will
kick in to convert `n` to an `Exp`.

## Decorators

The module `org.bitbucket.inkytonik.kiama.attribution.Decorators` provides _decorators_
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
def down[U](default : T => U)(a : T ==> U) : CachedAttribute[T, U] = {
```

The parameter `a` is a partial
function that defines the behaviour of the attribute at those
nodes where it is defined directly. Wherever `a` is not defined
the decorator will ask the parent for the value.
The parameter `default` is a function to use on a node that has
no parent.

`T ==> U` is an alias for the Scala library type `PartialFunction[T,U]`.
You can obtain the `==>` type constructor for use in your own code
by importing it from the `org.bitbucket.inkytonik.kiama` package:

```
import org.bitbucket.inkytonik.kiama.==>
```

There are other variants of `down` that a) take a default value
rather than a function, and b) throw an error if the root is reached
(but `a` is not defined there).
The `atRoot` decorator is the same as `down` with a default
function when `a` is not defined anywhere.

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
