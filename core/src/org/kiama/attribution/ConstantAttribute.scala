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
 * A constant attribute of a node type `T` with value of type `U`. The
 * value is given by the computation `u` which is evaluated at most once.
 */
class ConstantAttribute[T <: AnyRef,U] (name : String, u : => U) extends Attribute[T,U] (name) {

    /**
     * Lazily computed result of evaluating the attribute's computation.
     */
    private lazy val result = u

    /**
     * Return the value of this attribute for node `t`, always returning
     * `u` but only evaluating it once.
     */
    def apply (t : T) : U =
        result

}
