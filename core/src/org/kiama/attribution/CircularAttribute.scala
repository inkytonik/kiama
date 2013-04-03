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
 * Global state for the circular attribute evaluation algorithm
 * and the memoisation tables.
 */
private object CircularAttribute {
    var IN_CIRCLE = false
    var CHANGE = false
}

/**
 * An attribute of a node type `T` with value of type `U` which has a circular
 * definition.  The value of the attribute is computed by the function f
 * which may itself use the value of the attribute.  init specifies an
 * initial value for the attribute.  The attribute (and any circular attributes
 * on which it depends) are evaluated until no value changes (i.e., a fixed
 * point is reached).  The final result is memoised so that subsequent evaluations
 * return the same value.
 *
 * This code implements the basic circular evaluation algorithm from "Circular
 * Reference Attributed Grammars - their Evaluation and Applications", by Magnusson
 * and Hedin from LDTA 2003.
 */
class CircularAttribute[T <: AnyRef,U] (name : String, init : U, f : T => U) extends Attribute[T,U] (name) {

    import CircularAttribute._
    import java.util.IdentityHashMap

    /**
     * Has the value of this attribute for a given tree already been computed?
     */
    private val computed = new IdentityHashMap[T,Unit]

    /**
     * Has the attribute for given tree been computed on this iteration of the
     * circular evaluation?
     */
    private val visited = new IdentityHashMap[T,Unit]

    /**
     * The memo table for this attribute.
     */
    private val memo = new IdentityHashMap[T,U]

    /**
     * Return the value of the attribute for tree `t`, or the initial value if
     * no value for `t` has been computed.
     */
    private def value (t : T) : U = {
        val v = memo.get (t)
        if (v == null)
            init
        else
            v
    }

    /**
     * Return the value of this attribute for node `t`.  Essentially Figure 6
     * from the CRAG paper.
     */
    def apply (t : T) : U = {
        if (computed containsKey t) {
            value (t)
        } else if (!IN_CIRCLE) {
            IN_CIRCLE = true
            visited.put (t, ())
            var u = init
            do {
                CHANGE = false
                val newu = f (t)
                if (u != newu) {
                    CHANGE = true
                    u = newu
                }
            } while (CHANGE)
            visited.remove (t)
            computed.put (t, ())
            memo.put (t, u)
            IN_CIRCLE = false
            u
        } else if (! (visited containsKey t)) {
            visited.put (t, ())
            var u = value (t)
            val newu = f (t)
            if (u != newu) {
                CHANGE = true
                u = newu
                memo.put (t, u)
            }
            visited.remove (t)
            u
        } else
            value (t)
    }

}
