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
 * An attribute of a node type `T` with value of type `U`, supported by a circularity
 * test.  The value of the attribute is computed by the function `f`.  `f` will be
 * called each time the value of the attribute is accessed.  `f` should not itself
 * require the value of this attribute. If it does, a circularity error is reported
 * by throwing an `IllegalStateException`.
 */
class UncachedAttribute[T <: AnyRef,U] (name : String, f : T => U) extends Attribute[T,U] (name) {

    import java.util.IdentityHashMap
    import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}

    /**
     * Are we currently evaluating this attribute for a given tree?
     */
    private val visited = new IdentityHashMap[T,Unit]

    /**
     * Return the value of this attribute for node `t`, raising an error if
     * it depends on itself.
     */
    def apply (t : T) : U = {
        val i = start ("event" -> "AttrEval", "subject" -> t,
                       "attribute" -> this, "parameter" -> None,
                       "circular" -> false)
        if (visited containsKey t)
            reportCycle (t)
        else {
            visited.put (t, ())
            val u = f (t)
            visited.remove (t)
            finish (i, "value" -> u, "cached" -> false)
            u
        }
    }

}

