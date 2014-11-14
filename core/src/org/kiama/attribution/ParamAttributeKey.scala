/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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
 * Support for parameterised attributes: argument, node pair comparison.
 */
class ParamAttributeKey (val arg : Any, val node : Any) {

    import org.kiama.util.Comparison.same

    override def equals(o : Any) : Boolean =
        o match {
            case o : ParamAttributeKey =>
                arg == o.arg && same (node, o.node)
            case _ =>
                false
        }

    override def hashCode : Int =
        System.identityHashCode (node) ^ arg.hashCode

}
