# Kiama's relations

Up: [User Manual](UserManual), Prev: [Attribution](Attribution), Next: [Machines](Machines)

This page provides an overview of Kiama's support for
_relations_.
Relations are particularly useful when exploring tree structures, such as in attribute grammar equations.
For information about Kiama's attribute grammar features, see [Attribution](Attribution).

All of the Kiama examples use relations as of Kiama 2.0.

## Introduction

Consider the simple grammar used in Kiama's Repmin example, as represented by Scala case classes:

```
sealed abstract class RepminTree extends Product
case class Fork(left : RepminTree, right : RepminTree) extends RepminTree
case class Leaf(value : Int) extends RepminTree
```

and a typical instance of a Repmin tree, constructed using intermediate values so we have a name for each node:

```
val a = Leaf(1)
val b = Leaf(2)
val c = Leaf(1)
val d = Fork(b, c)
val e = Fork(a, d)
```

This code will bind `e` to `Fork(Leaf(1),Fork(Leaf(2),Leaf(1)))`.

When defining processing for trees of this form, we need to be able to understand the relationships between nodes.
For example, `e` is the parent of `a` and `d`, `b` is the previous node at the same level as `c`, and so on.
Understanding these relationships is necessary since two nodes that are equal values such as `a` and `c` (which are both `Leaf(1)`) may have different attribute values because they are at different places in the tree.
For example, consider an attribute that gives a node's depth (i.e., the distance of the node from the root).
In our example, Node `a` is at depth 1, but node `c` is at depth 2.

Kiama relations help record these sorts of relationships between nodes.
Tree relations can be used in pattern matching so that context can be used to guide attribute equation choice.

## Relations in the nodes

Before we discuss how Kiama relations work, it is worth thinking about the obvious approach: store relational information in the tree nodes.
For example, it is a relatively easy matter to add a `parent` field to each node that points to the parent of that node.
Or perhaps it should be an option so that we can represent the case when the parent is not there (i.e., the node is the root of the tree).

Kiama 1.x worked in this way, but deficiencies were apparent.
First, the classes that represent nodes somehow have to have a parent field, not to mention fields that represent other relations such as previous, next, etc.
Ideally we would be able to use Kiama on classes that come from third parties, but putting relations in the classes requires us to modify those classes or extend them.
Second, even if we were prepared to modify or extend the classes, we still run into problems when performing operations such as rewriting where node sharing is common.
If a node in the input of a rewrite is also present in the output of that rewrite, it could have different parents in those two structures, so a single parent field is not sufficient.

## Tree relations

Kiama 2.x moved to an approach that represents tree relations outside of the tree structure itself.
Thus, the node classes do not need to be modified or extended, even when new relations are added.
A single node can be shared by more than one structure since the shared thing is the node, not its relationships to other nodes.

The Kiama `Tree[T,R]` type represents relations for a tree structure whose nodes are of type `T` and whose root is of type `R`.
An instance for a particular tree structure can be created by passing the root of the structure to the `Tree` constructor.
For example, we can create a `Tree` for our example rooted at `e` as follows:

```
import org.bitbucket.inkytonik.kiama.relation.Tree

val tree = new Tree[RepminTree,RepminTree](e)
```

(It can be useful to introduce a type alias for the `Tree[RepminTree,RepminTree]` type to save some typing.)
In this case the root type is not special, but for many languages we know that the root is of a particular type, so that type would be passed as the second type parameter instead of the common node type.

## Controlling tree shapes

The `Tree` constructor also takes an optional second argument called `shape`.
By default, it is `LeaveAlone` that means the `Tree` library assumes that the passed tree structure is fine as it is.

One alternative shape is `CheckTree` that causes `Tree` to make sure that your tree structure actually is a tree (i.e., no sharing) and throw an exception if it isn't.
This option is useful if the source of your structure is not trusted and you just want to stop if a non-tree is provided.

The other alternative shape is `EnsureTree` that will clone nodes as necessary to remove sharing.
This option is useful if your source is a rewriting pass where sharing may have been introduced.
`Tree` will remove the sharing for you.

The shapes can be imported from package `org.bitbucket.inkytonik.kiama.relation`.

## Relational tree operations

A `Tree` instance provides operations that can be used to interrogate the relationships between nodes in the tree structure that it represents.
For example, `tree.parent(n)` will return a vector of the nodes that are in the `parent` relation with node `n`.
The vector will contain at most one node if the structure is actually a tree.
An exception will be thrown if `n` isn't a part of the tree structure.

Similarly, there are operations `child`, `firstChild`, `lastChild`, `prev`, `next` and `sibling` that can be used to get all children, the first child, the last child, the previous node at the same leve, the next node at the same level and sibling nodes, respectively.

## Using relational tree operations as patterns

While the relational tree operations can be called directly, the easiest way to use them is via pattern matching.
Any relation can be used as a pattern that succeeds if the relation contains related nodes that match sub-patterns.
For example, if you want to do something if a node has a parent and something else if it doesn't use the pattern `tree.parent(p)` as follows.

```
   n match {
       case tree.parent(p) =>
           // p is n's parent
       case _ =>
           // n has no parent
   }
```

Similarly, the pattern `tree.next(x)` will succeed if `n` is not the last node at its level, binding `x` to the next node.

Arbitrary sub-patterns can be used to discriminate contexts further.
The pattern `tree.next(x : Leaf)` is the same as in the previous example, but will fail if the next node is not a `Leaf` node.
This example could also be written `tree.next(Leaf(_))` if a binding to the next node is not needed.

Nesting of tree relations is also possible.
The pattern `tree.parent(tree.next(x : Leaf))` succeeds only if there is a parent and the parent has a next sibling that is a `Leaf`.

Relations also come with a `pair` method that can be used to match against the node and its image in the relation.
For example, the pattern `tree.parent.pair(n, p)` will succeed if the node being matched against has a parent.
It will bind `n` to the node and `p` to the parent.
This form is useful if you need a reference to the node but don't have it from elsewhere (e.g., you are defining a partial function literal using pattern matching).
It is also useful if you want to restrict the pattern to specific situations, such as `tree.parent.pair(n : Foo, p : Bar)` that will only succeed if the node being matched is a `Foo` that has a `Bar` parent.

## Other tree operations

In addition to the relations themselves, `Tree` has a few convenience methods. `tree.isRoot(n)` returns a Boolean that indicates whether `n` is the root of the tree structure or not. Similarly, there are `isFirst` and `isLast` that can be used to test whether a node is first or last at its level. Finally, there is `index` that returns the integer index starting at zero for the node's position at its level.

## Analysis modules

It is common to want to define a collection of functions that need to share a `Tree` value.
For example, this need arises when defining attributes that compute semantic information for a programming language tree.
It is important to be sure that the attributes are all using the same tree relations.

It is convenient to define the functions or attributes in a class that takes the tree as a constructor argument and therefore is automatically visible to all of them.
Thus, the following is a typical usage:

```
import org.bitbucket.inkytonik.kiama.relation.Tree
import my.compiler.SemanticAnalyser

val tree = new Tree[RepminTree,RepminTree](e)
val analyser = new SemanticAnalyser(tree)
... use analyser.foo attribute ...
```

where `SemanticAnalyser` has the form:

```
class SemanticAnalyser(tree : Tree[RepminTree,RepminTree]) {

    val foo =
        attr {
            case tree.parent(p) =>
                ...
            ... other cases ...
        }

    ... other attributes ...

}
```

Up: [User Manual](UserManual), Prev: [Attribution](Attribution), Next: [Machines](Machines)
