/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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

package org.kiama
package attribution

/**
 * Common functionality for classes whose instances are to be attributed.
 *
 * This trait must be extended by all classes for which the node properties
 * such as `parent` or the attribute shorthand notation `->` are desired.
 * Also provides deep and shallow cloning support.
 */
trait Attributable extends Product with Cloneable {

    /**
     * A link to the parent `Attributable` node of this node or `null` if
     * this node has no parent.  Note that this link will skip intervening
     * non-`Attributable` ancestors.
     *
     * @see initTreeProperties
     */
    var parent : Attributable = null

    /**
     * A short-hand for `parent.asInstanceOf[T]`, which is useful in cases
     * a `T`-specific operation is applied to the parent, which otherwise
     * would be of type `Attributable`.
     */
    def parent[T] : T = parent.asInstanceOf[T]

    /**
     * Is this node the root of the hierarchy?
     */
    def isRoot : Boolean = parent == null

    /**
     * A link to the child of the same `Attributable` parent immediately to the
     * left of this one, or `null` if this is the first child of its parent.
     */
    var prev : Attributable = null

    /**
     * A short-hand for `prev.asInstanceOf[T]`, which is useful in cases
     * a `T`-specific operation is applied to the `prev`, which otherwise
     * would be of type `Attributable`.
     */
    def prev[T] : T = prev.asInstanceOf[T]

    /**
     * A link to the child of the same `Attributable` parent immediately to the
     * left of this one, or `null` if this is the first child of its parent.
     */
    var next : Attributable = null

    /**
     * A short-hand for `next.asInstanceOf[T]`, which is useful in cases
     * a `T`-specific operation is applied to the `next`, which otherwise
     * would be of type `Attributable`.
     */
    def next[T] : T = next.asInstanceOf[T]

    /**
     * Is this node the first child of its parent?
     */
    def isFirst : Boolean = prev == null

    /**
     * Is this node the last child of its parent?
     */
    def isLast : Boolean = next == null

    /**
     * The index of this node as a child of its parent or -1 if this
     * node has no parent (i.e., it's a root).
     */
    var index : Int = -1

    /**
     * This node's `Attributable` children in left-to-right order.  Children
     * that are not `Attributable` are ignored, except for nodes that collect
     * `Attributable` children. Those indirect children are also collected
     * here.
     *
     * @see initTreeProperties
     */
    def children : Iterator[Attributable] =
        _children.iterator

    /**
     * Does this node have some `Attributable` children?
     */
    def hasChildren : Boolean = _children.nonEmpty

    /**
     * This node's first `Attributable` child.
     * Raises an `IndexOutOfBounds` exception if this node has no such children.
     */
    def firstChild[T] : T = _children (0).asInstanceOf[T]

    /**
     * This node's last `Attributable` child.
     * Raises an `IndexOutOfBounds` exception if this node has no such children.
     */
    def lastChild[T] : T = _children (_children.length - 1).asInstanceOf[T]

    /**
     * Record of this node's `Attributable` children.
     */
    private val _children = new scala.collection.mutable.ListBuffer[Attributable]

    /**
     * Reference an attribute or function that can be applied to this node.
     * `this->attribute` is equivalent to `attribute(this)`.
     */
    @inline
    final def ->[U] (a : this.type => U) : U =
        a (this)

    /**
     * Reference an attribute or function that can be applied to this node.
     * `this->attribute` is equivalent to `attribute(this)`.
     * The attribute definition is defined on a type `T` other than that of the
     * node to which it is applied (`this.type`).  An implicit value must exist
     * to transform from the node type to the type expected by the attribute.
     * This form of attribute reference is commonly used to implement attribute
     * forwarding where the implicit parameter enables references to the attribute
     * to be implicitly forwarded to some other node.
     */
    @inline
    final def ->[T,U] (a : T => U) (implicit b : this.type => T) : U =
        a (b (this))

    /**
     * House-keeping method to connect this node's children to it and their
     * siblings (and recursively through the subtree rooted here). The easy
     * case is `Attributable` children that are direct descendants.
     * Also connected are `Attributable` descendants that are reachable via
     * a path of descendants that only passes through `GenTraversable`, `Some`,
     * or tuple (up to size four) nodes.  Thus, descendants of these kinds
     * are regarded as children for the purposes of attribution.  As a
     * side-effect, this method remembers the children so that they can be
     * accessed easily via the children iterator.
     */
    def initTreeProperties () {

        import scala.collection.GenTraversable

        var ind : Int = 0
        var prev : Attributable = null

        /**
         * Set the node connections and index of `c`.
         */
        def setConnections (c : Attributable) {
            c.parent = this
            _children += c
            c.index = ind
            ind += 1
            c.prev = prev
            c.next = null
            if (prev != null) prev.next = c
            prev = c

            // Recursively set the connections below c
            c.initTreeProperties
        }

        /**
         * Recursively set the child connections of `node` and its
         * `Attributable` children, skipping any number of `Some` options,
         * `Left` or `Right` eithers, tuples, or `GenTraversable` nodes
         * on the way.
         */
        def setNodeChildConnections (node : Any) : Unit =
            node match {
                case c : Attributable =>
                    setConnections (c)
                case Some (o) =>
                    setNodeChildConnections (o)
                case Left (l) =>
                    setNodeChildConnections (l)
                case Right (r) =>
                    setNodeChildConnections (r)
                case (a, b) =>
                    setNodeChildConnections (a)
                    setNodeChildConnections (b)
                case (a, b, c) =>
                    setNodeChildConnections (a)
                    setNodeChildConnections (b)
                    setNodeChildConnections (c)
                case (a, b, c, d) =>
                    setNodeChildConnections (a)
                    setNodeChildConnections (b)
                    setNodeChildConnections (c)
                    setNodeChildConnections (d)
                case s : GenTraversable[_] =>
                    for (v <- s)
                        setNodeChildConnections (v)
                case _ =>
                    // Ignore other kinds of nodes
            }

        // Start by setting the connections of the fields of this.
        _children.clear ()
        for (c <- productIterator)
            setNodeChildConnections (c)

    }

    /**
     * Make a shallow clone of this node.
     *
     * @see Attributable.deepclone
     */
    override def clone () : Attributable =
        super.clone ().asInstanceOf[Attributable]

}

/**
 * Support for the `Attributable` class.
 */
object Attributable {

    /**
     * Deep clone the given `Attributable` tree.
     */
    def deepclone[T <: Attributable] (t : T) : T = {

        import org.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}

        val deepcloner =
            everywherebu (rule {
                case n : Attributable if !n.hasChildren =>
                    n.clone ()
            })

        rewrite (deepcloner) (t)

    }

}
