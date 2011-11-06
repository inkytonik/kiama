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
    def down[T <: Attributable,U] (a : T ==> U) : T => U =
        attr[T,U] {
            case t =>
                if (a.isDefinedAt (t))
                    a (t)
                else
                    (down (a)) (t.parent[T])
        }

    /**
     * A pair of attributes that thread through a tree in a depth-first
     * left-to-right fashion.  The in (out) attributes provides the value
     * of the chain as it enters a node from above (leaves a node from 
     * below).  The chain as a function is the same as the out attribute.
     */
    case class Chain[T,U] (in : (T => U), out : (T => U)) extends (T => U) {
        def apply (t : T) = out (t)
    }

    /**
     * An identity function for chain updates.
     */
    private def idf[T,U] : (T => U) => (T ==> U) =
        f => { case t => f (t) }

    /**
     * Create a new attribute chain.  The update functions provide ways to 
     * influence the chain value, by taking the default computation of the
     * in or out attribute and returning a partial function.  If the domain
     * of the partial function contains a node, then that function is used
     * to compute the chain value at the node n, otherwise the default chain
     * attribute is used.  If an update function is omitted, it defaults 
     * to the identity.
     */
    def chain[T <: Attributable,U] (
                 inupdate : (T => U) => (T ==> U) = idf,
                 outupdate : (T => U) => (T ==> U) = idf
             ) : Chain[T,U] = {

        def update (dflt : T => U, upd : (T => U) => (T ==> U)) : T => U =
            attr {
                case t =>
                    val f = upd (dflt)
                    if (f.isDefinedAt (t))
                        f (t)
                    else
                        dflt (t)
            }

        lazy val indflt : T => U =
            attr {
                case t if t.isRoot =>
                    sys.error ("chain indflt: root of tree reached at " + t)
                case t if t.isFirst =>
                    (t.parent[T])->in
                case t =>
                    (t.prev[T])->out
            }

        lazy val in : T => U =
            update (indflt, inupdate)

        lazy val outdflt : T => U =
            attr {
                case t if t.hasChildren =>
                    (t.lastChild[T])->out
                case t =>
                    t->in
            }

        lazy val out : T => U =
            update (outdflt, outupdate)

        Chain (in, out)
    }

}