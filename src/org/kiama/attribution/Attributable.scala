/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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

import scala.util.parsing.input.Positional

/**
 * Common functionality for all classes that are to be attributed.  This
 * trait must be extended by all classes for which the node properties
 * such as parent and the attribute shorthand notation <code>-></code>
 * are desired.
 */
trait Attributable extends Product with Positional {

    /**
     * A link to the parent Attributable node of this node or null if this
     * node has no parent.  Note that this link will skip intervening
     * non-Attributable ancestors, such as <code>Option</code> or
     * <code>Seq</code> nodes.
     */
    var parent : Attributable = null

    /**
     * A short-hand for parent.asInstanceOf[T], which is useful in cases
     * a T-specific operation is applied to the parent, which otherwise
     * would be Attributable.
     */
    def parent[T] : T = parent.asInstanceOf[T]

    /**
     * Is this node the root of the hierarchy?
     */
    def isRoot : Boolean = parent == null

    /**
     * A link to the child of the same Attributable parent immediately to the
     * left of this one, or null if this is the first child of its parent.
     */
    def prev[T] : T = _prev.asInstanceOf[T]

    /**
     * Private field backing prev to make the types work correctly.
     */
    private var _prev : Attributable = null

    /**
     * A link to the child of the same Attributable parent immediately to the right
     * of this one, or null if this is the last child of its parent.
     */
    def next[T] : T = _next.asInstanceOf[T]

    /**
     * Private field backing next to make the types work correctly.
     */
    private var _next : Attributable = null

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
     * node has no parent (is root).
     */
    var index : Int = -1

    /**
     * This node's attributable children in left-to-right order.  Children
     * that are not Attributable are ignored, except for sequences (<code>Seq[_]</code>)
     * and optional children (<code>Option[_]</code>).  In the case of sequences and
     * options, their contents are processed and any immediate Attributable
     * contents are included in the sequence.
     */
    def children : Iterator[Attributable] =
        _children.iterator

    /**
     * If this node has some attributable children then return true, else return false.
     */
    def hasChildren : Boolean = !_children.isEmpty

    /**
     * This node's first attributable child.
     * Raises an IndexOutOfBounds exception if this node has no children
     */
    def firstChild[T] : T = _children(0).asInstanceOf[T]

    /**
     * This node's last attributable child.
     * Raises an IndexOutOfBounds exception if this node has no children
     */
    def lastChild[T] : T = _children(_children.length - 1).asInstanceOf[T]

    /**
     * Record of this node's attributable children.
     */
    private val _children = new scala.collection.mutable.ListBuffer[Attributable]

    /**
     * Reference an attribute or function that can be applied to this node.
     * <code>this->attribute</code> is equivalent to <code>attribute(this)</code>.
     */
    @inline
    final def ->[U] (a : this.type => U) = a (this)

    /**
     * Reference an attribute or function that can be applied to this node.
     * <code>this->attribute</code> is equivalent to <code>attribute(this)</code>.
     * The attribute definition is defined on a type other than that of the
     * node to which it is applied.  An implicit value must exist to transform
     * from the node type to the type expected by the attribute.  This form
     * of attribute reference is commonly used to implement attribute forwarding
     * where the implicit parameter enables references to the attribute to be
     * implicitly forwarded to some other node.
     */
    @inline
    final def ->[T,U] (a : T => U) (implicit b : this.type => T) =
        a (b (this))

    /**
     * House-keeping method to connect my children to me and their siblings.
     * If a node is a direct child of a <code>Seq</code> or <code>Some</code>,
     * then the parent link "bypasses" that parent to go to the <code>Attributable</code>
     * parent above.  It is assumed at that sequences and options are not directly nested.
     * As a side-effect, this method remembers the attributable children
     * so that they can be accessed easily via the children iterator.
     */
    private def setChildConnections () = {

        var ind : Int = 0
        var prev : Attributable = null
        def setConnections(c : Attributable) {
           c.parent = this
           _children += c
           c.index = ind
           ind += 1
           c._prev = prev
           if (prev != null) prev._next = c
           prev = c
        }

        for (i <- 0 until productArity) {
            productElement (i) match {
                case c : Attributable => setConnections(c)
                case Some(c : Attributable) => setConnections(c)
                case s : Seq[_] => {
                    for (v <- s) {
                        v match {
                            case c : Attributable => setConnections(c)
                            case _ =>
                                // Ignore elements that are non-Attributables
                        }
                    }
                }
                case _ =>
                    // Ignore children that are not Attributable, options or sequences
            }
        }

    }

    setChildConnections

}
