/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2017 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.bitbucket.inkytonik.kiama
package relation

/**
 * Exception thrown if a tree relation operation tries to use a node that
 * is not in the tree to which the relation applies.
 *
 * For the time-being, the exception does not indicate which tree is involved
 * because it's not clear how to identify the tree briefly in a short message.
 * The stack trace that accompanies the exception should be sufficient.
 */
case class NodeNotInTreeException[T](t : T) extends Exception(s"node not in tree: $t")

/**
 * A bridge node in a tree structure which connects to another structure.
 * Bridges are not traversed when determining the tree structure.
 */
case class Bridge[T](cross : T)

/**
 * Tree properties
 */
abstract class TreeProp

/**
 * When creating the child relation for the structure, check that it is
 * in fact a tree. A system error will result if it's not.
 */
case object CheckTree extends TreeProp

/**
 * When creating the tree, laziy clone the structure so that there are no
 * duplicate nodes.
 */
case object LazyClone extends TreeProp

/**
 * Relational representations of trees built out of hierarchical `Product`
 * instances. Typically, the nodes are instances of case classes.
 *
 * The type `T` is the common base type of the tree nodes. The type `R` is a
 * sub-type of `T` that is the type of the root node.
 *
 * The `originalRoot` value should be the root of a finite, acyclic structure
 * that contains only `Product` instances of type `T` (unless they are skipped,
 * see below).
 *
 * The `props` argument is a sequence of `TreeProp` values, defaulting to an
 * empty sequence.
 *
 * If `LazyClone` is in `props` then the structure reachable from `originalRoot`
 * will be processed to ensure that it is a tree structure. I.e., nodes will
 * be cloned if they are shared. If the structure reachable from `originalRoot`
 * is actually a tree (i.e., contains no shared nodes) then the field `root`
 * will be the same as `originalRoot`.
 *
 * If `LazyClone` is not in `props`, then the structure will be left alone
 * and `root` will always be the same as `originalRoot`.
 *
 * If `EnsureTree` is in `props` then a dynamic check will be performed when
 * the tree relations are created to ensure that structure is actually a tree.
 * If a node has more than one parent, then an error will be thrown.
 *
 * If `EnsureTree` is not in `props` then there will be no attempt to check the
 * tree structure.
 *
 * The `child` relation of a tree is defined to skip certain nodes.
 * Specifically, if a node of type `T` is wrapped in a `Some` of an option,
 * a `Left` or `Right` of an `Either`, a tuple up to size four, or a
 * `TraversableOnce`, then those wrappers are ignored. E.g., if `t1`
 * is `Some (t2)` where both `t1` and `t2` are of type `T`, then `t1`
 * will be `t2`'s parent, not the `Some` value.
 *
 * Thanks to Len Hamey for the idea to use lazy cloning to restore the tree
 * structure instead of requiring that the input trees contain no sharing.
 */
class Tree[T <: Product, +R <: T](val originalRoot : R, props : Seq[TreeProp] = Seq()) {

    tree =>

    import org.bitbucket.inkytonik.kiama.relation.Relation.emptyImage
    import org.bitbucket.inkytonik.kiama.relation.TreeRelation
    import org.bitbucket.inkytonik.kiama.relation.TreeRelation.childFromTree
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy
    import org.bitbucket.inkytonik.kiama.rewriting.Cloner.lazyclone
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{all, attempt, rule}
    import org.bitbucket.inkytonik.kiama.util.Comparison
    import org.bitbucket.inkytonik.kiama.util.Comparison.same

    /**
     * A version of `bottomup` that doesn't traverse bridges.
     */
    def bottomupNoBridges(s : Strategy) : Strategy =
        rule[Bridge[_]] { case b => b } <+
            (all(bottomupNoBridges(s)) <* s)

    /**
     * A version of `everywherebu` that doesn't traverse bridges.
     */
    def everywherebuNoBridges(s : Strategy) : Strategy =
        bottomupNoBridges(attempt(s))

    /**
     * The root node of the tree. The root node will be different from the
     * original root if any nodes in the original tree are shared, since
     * they will be cloned as necessary to yield a proper tree structure.
     * If there is no sharing then `root` will be same as `originalRoot`.
     * Bridges to other structures will not be traversed.
     */
    lazy val root =
        if (props.contains(LazyClone))
            lazyclone(originalRoot, everywherebuNoBridges)
        else
            originalRoot
    /**
     * The basic relations between a node and its children. All of the
     * other relations are derived from `child` and `parent`.
     */
    lazy val child : TreeRelation[T] = {

        /*
         * The child relation for this tree.
         */
        val child = childFromTree(tree)

        // As a safety check, we make sure that values are not children
        // of more than one parent.
        if (props.contains(CheckTree)) {
            val msgBuilder = new StringBuilder
            val parent = child.inverse
            for (c <- parent.domain) {
                val ps = parent(c)
                if (ps.length > 1) {
                    msgBuilder ++= s"child $c has multiple parents:\n"
                    for (p <- ps) {
                        msgBuilder ++= s"  $p\n"
                    }
                }
            }
            if (!msgBuilder.isEmpty)
                sys.error("Tree creation: illegal tree structure:\n" + msgBuilder.result)
        }

        // All ok
        child

    }

    /**
     * The nodes that occur in this tree. Mostly useful if you want to
     * iterate to look at every node.
     */
    lazy val nodes : Vector[T] =
        child.domain

    /**
     * If the tree contains node `u` return `v`, otherwise throw a
     * `NodeNotInTreeException`. `v` is only evaluated if necessary.
     */
    def whenContains[V](t : T, v : => V) : V =
        if (same(t, root) || (parent.containsInDomain(t)))
            v
        else
            throw new NodeNotInTreeException(t)

    // Derived relationsc

    /**
     * Map the function `f` over the images of this tree's child relation and
     * use the resulting graph to make a new tree relation.
     */
    def mapChild(f : Vector[T] => Vector[T]) : TreeRelation[T] = {
        val relation = new TreeRelation(tree)
        for (t <- child.domain)
            relation.set(t, f(child(t)))
        relation
    }

    /**
     * A relation that relates a node to its first child.
     */
    lazy val firstChild : TreeRelation[T] =
        mapChild(_.take(1))

    /**
     * A relation that relates a node to its last child.
     */
    lazy val lastChild : TreeRelation[T] =
        mapChild(_.takeRight(1))

    /**
     * The parent relation for this tree (inverse of child relation).
     */
    lazy val parent : TreeRelation[T] =
        child.inverse

    /**
     * A relation that relates a node to its next sibling. Inverse of
     * the `prev` relation.
     */
    lazy val next : TreeRelation[T] = {
        val relation = new TreeRelation(tree)
        relation.set(root, emptyImage)
        for (t <- child.domain) {
            val children = child(t)
            if (children.length > 0) {
                for (i <- 0 until children.length - 1)
                    relation.set(children(i), Vector(children(i + 1)))
                relation.set(children(children.length - 1), emptyImage)
            }
        }
        relation
    }

    /**
     * A relation that relates a node to its previous sibling. Inverse of the
     * `next` relation.
     */
    lazy val prev : TreeRelation[T] =
        next.inverse

    /**
     * A relation that relates a node to its siblings, including itself.
     */
    lazy val sibling : TreeRelation[T] = {
        val relation = new TreeRelation(tree)
        relation.put(tree.root, tree.root)
        for (t <- parent.domain; p <- parent(t))
            relation.set(t, child(p))
        relation
    }

    // Predicates derived from the relations

    /**
     * Return the first index of `t` in the children of `t's` parent node.
     * Counts from zero. Is zero for root.
     */
    def index(t : T) : Int =
        parent(t) match {
            case Vector(p) =>
                Comparison.indexOf(child(p), t)
            case _ =>
                if (same(t, root))
                    0
                else
                    throw new NodeNotInTreeException(t)
        }

    /**
     * Return the last index of `t` in the children of `t's` parent node.
     * Counts from zero. Is zero for root.
     */
    def indexFromEnd(t : T) : Int =
        parent(t) match {
            case Vector(p) =>
                val c = child(p)
                c.length - Comparison.lastIndexOf(c, t) - 1
            case _ =>
                if (same(t, root))
                    0
                else
                    throw new NodeNotInTreeException(t)
        }

    /**
     * Return whether or not `t` is a first child. True for root.
     */
    def isFirst(t : T) : Boolean =
        prev(t).isEmpty

    /**
     * Return whether or not `t` is a last child. True for root.
     */
    def isLast(t : T) : Boolean =
        next(t).isEmpty

    /**
     * Return whether or not `t` is the root of this tree.
     */
    def isRoot(t : T) : Boolean =
        parent(t).isEmpty

}
