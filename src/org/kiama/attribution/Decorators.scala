/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2012 Anthony M Sloane, Macquarie University.
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
     *
     * If the root of the tree is reached without a definition being supplied
     * for the chain, a runtime exception is thrown. Both of the components
     * of the chain are reset to avoid errors for cyclic if the exception is
     * caught and they are subsequently evaluated again.
     */
    def chain[T <: Attributable,U] (
                 inupdate : (T => U) => (T ==> U) = idf[T,U],
                 outupdate : (T => U) => (T ==> U) = idf[T,U]
             ) : Chain[T,U] = {

        def error (t : T) : Nothing = {
            in.reset       
            out.reset
            sys.error ("chain root of tree reached at " + t)
        }

        def indflt (t : T) : U =
        	if (t.isRoot)
                error (t)
            else if (t.isFirst)
                in (t.parent[T])
            else
                out (t.prev[T])

        lazy val infunc = inupdate (indflt)

        lazy val in : CachedAttribute[T,U] =
            attr (t => {
                if (infunc.isDefinedAt (t))
                    infunc (t)
                // inline indflt here to save call, really is
                // else indflt (t)
                else if (t.isRoot)
                	error (t)
                else if (t.isFirst)
                	in (t.parent[T])
                else
                	out (t.prev[T])
            })

        def outdflt (t : T) : U =
            if (t.hasChildren)
                out (t.lastChild[T])
            else  
                in (t)

        lazy val outfunc = outupdate (outdflt)

        lazy val out : CachedAttribute[T,U] =
            attr (t => {
                if (outfunc.isDefinedAt (t))
                    outfunc (t)
                // Inline outdflt here to save call, really is
                // else outdflt (t)
                else if (t.hasChildren)
                	out (t.lastChild[T])
                else  
                	in (t)
            })

        Chain (in, out)
    }

}