/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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
package util

/**
 * Utility module for comparison routines.
 */
object Comparison {

    /**
     * Compare two arbitrary values. If they are both references, use
     * reference equality, otherwise use value equality.
     */
    def same (v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (d1 : Double, d2 : Double) =>
                    d1 == d2
                case (f1 : Float, f2 : Float) =>
                    f1 == f2
                case (i1 : Int, i2 : Int) =>
                    i1 == i2
                case (l1 : Long, l2 : Long) =>
                    l1 == l2
                case (r1 : AnyRef, r2: AnyRef) =>
                    r1 eq r2
                case _ =>
                    sys.error (s"same: comparison of $v1 and $v2, should not be reached")
            }

    /**
     * As for `same`, except that if the two values are `Some` options
     * containing references, they are unwrapped first and the contents are
     * compared by reference.
     */
    def optsame (v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (Some (r1 : AnyRef), Some (r2 : AnyRef)) =>
                    r1 eq r2
                case _ =>
                    same (v1, v2)
            }

}
