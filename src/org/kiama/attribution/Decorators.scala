/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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

object Decorators {

    import org.kiama.attribution.Attribution._
    import org.kiama.attribution.Attributable

    /**
     * A decorator that propogates an attribute value down the tree.
     */
    def down[T <: Attributable,U] (a : T ==> U) : T ==> U =
        attr {
            case t =>
                if (a.isDefinedAt (t))
                    a (t)
                else
                    (down (a)) (t.parent[T])
        }

}