# Processing collections with Kiama

Up: [Context](Context.md), Prev: [Case Classes](CaseClasses.md), Next: [Rewritable](Rewritable.md)

It is often the case that data to be processed by Kiama is formed using Scala
[collection classes](http://www.scala-lang.org/docu/files/collections-api/collections.html),
such as sets, vectors and maps.

Structures encoded using collections can be [rewritten](Rewriting.md) or
can have [attribute values](Attribution.md) defined for them.

Attribution is defined as usual by attributes that are independent of
the data structure.

Rewriting of collections is currently supported for any collection
that is `GenTraversable` or is a `Map`, relying on the `foreach` method
to gain access to elements of the collection. Also, the [Rewritable](Rewritable.md)
class provides an interface that can be used to implement rewriting
of arbitrary structures.

For example, consider the following structure

```
val m1 = Map (Set (1, 3) -> 0, Set (2, 4, 6) -> 0)
val m2 = Map (Set (12, 16) -> 0, Set (23) -> 0)
val l = List (m1, m2)
```

which is a list of maps where the keys are sets and the values are
integers (all zero).

We can rewrite the data structure `l` as follows to replace the zero
values in the map with the sizes of the sets that are the map keys.

```
val strat = rule { case (s : Set[_], _) => (s, s.size) }
rewrite (everywhere (strat)) (l)
```

The result is

```
List(Map(Set(1, 3) -> 2, Set(2, 4, 6) -> 3), Map(Set(12, 16) -> 2, Set(23) -> 1))
```

(Note that a `List` is `Traversable`, but Kiama treats it as a
`Product` so that its elements are regarded as immediate descendants
of the list, not as descendants of the constituent `::` values.)

At the moment, rewriting of collections is only available for
sequential collections. We are working on an update to extend this
support to parallel collections.

## Collections and `Attributable` node properties

Collections interact in a special way with the node properties provided
by the [Attributable](Attribution#Attributable.md) class. Specifically,
if a collection is present in the tree structure, it will be ignored
when it comes to determining node parents and children. The motivation
is that usually you don't want to attach attributes to the collections
but to the problem-specific nodes that contain the collections, since
the latter contribute the semantic structure.

For example, suppose that a problem-specific node `P` contains a list child
that in turn contains `C` nodes. The `P` node will act as the container of
the `C` nodes, even though the list is actually holding them. The `C` nodes
will have their `parent` properties set to the `P` node, not the list.
Similarly, the `P` node will have the `C` nodes in its `children` property.

Up: [Context](Context.md), Prev: [Case Classes](CaseClasses.md), Next: [Rewritable](Rewritable.md)
