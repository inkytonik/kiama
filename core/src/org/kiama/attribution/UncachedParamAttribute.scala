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
 * A variation of the `UncachedAttribute` class for parameterised attributes.
 */
class UncachedParamAttribute[A,T <: AnyRef,U] (name : String, f : A => T => U) extends (A => Attribute[T,U]) {

    attr =>

    import java.util.IdentityHashMap

    /**
     * Are we currently evaluating this attribute for a given argument and tree?
     */
    private val visited = new IdentityHashMap[ParamAttributeKey,Unit]

    /**
     * Return the value of this attribute for node `t`, raising an error if
     * it depends on itself.
     */
    def apply (arg : A) : Attribute[T,U] =
        new Attribute[T,U] (name + " (" + arg + ")") {

            def apply (t : T) : U = {
                val key = new ParamAttributeKey (arg, t)
                if (visited containsKey key) {

                    throw new IllegalStateException ("Cycle detected in attribute evaluation")
                } else {
                    visited.put (key, ())
                    val u = f (arg) (t)
                    visited.remove (key)
                    u
                }
            }

        }

}
