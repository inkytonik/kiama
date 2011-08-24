/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2011 Anthony M Sloane, Macquarie University.
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
     * A decorator that propagates an attribute value down the tree.
     */
    def down[T <: Attributable,U] (a : T ==> U) : T ==> U =
        attr {
            case t =>
                if (a.isDefinedAt (t))
                    a (t)
                else
                    (down (a)) (t.parent[T])
        }

    /**
     * A decorator that propagates an attribute value in left-to-right postorder
     * through a tree.  init is a partial function that provides initial values
     * for the chain at selected nodes.  update is a function that transforms
     * a default partial function for the chain value into a partial function 
     * that provides an updated value at selected nodes.
     * 
     * chain returns a pair of attributes that can be used to access the value
     * of the chain as it enters (resp. leaves) any node.
     */
    def chain[T <: Attributable,U] (init : T ==> U) (update : (T ==> U) => T ==> U) :
            (T ==> U, T ==> U) = {
         
        lazy val indflt : T ==> U =
            attr {
                case t if t.isFirst =>
                    (t.parent[T])->inattr
                case t =>
                    (t.prev[T])->outattr
            }

        lazy val inattr : T ==> U =
            attr (init orElse indflt)
            
        lazy val outdflt : T ==> U =
            attr {
                case t if t.hasChildren =>
                    (t.lastChild[T])->outattr
                case t =>
                    t->inattr
            }

        lazy val outattr : T ==> U =
            attr {
                case t =>
                    ((update (outdflt)) orElse outdflt) (t)
            }
                  
        (inattr, outattr)

    }

}