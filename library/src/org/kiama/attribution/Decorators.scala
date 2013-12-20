/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2013 Anthony M Sloane, Macquarie University.
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
 * Decorators are higher-order operations that provide common patterns of
 * tree attribution based on simple attributes or functions.
 */
object Decorators {

    import org.kiama.attribution.Attribution._
    import org.kiama.attribution.Attributable
    import org.kiama.rewriting.Rewriter.{collectl => rwcollectl,
        collects => rwcollects}

    /**
     * A cached attribute that wraps the `collectl` rewriting combinator.
     * Run `f` in a top-down left-to-right traversal of the sub-tree to
     * which the attribute is applied. Accumulate the values produced
     * at the places where `f` is defined into a list and return the
     * list.
     */
    def collectl[U] (f : Any ==> U) : CachedAttribute[Any,List[U]] =
        attr (rwcollectl (f))

    /**
     * A cached attribute that wraps the `collects` rewriting combinator.
     * Run `f` in a top-down left-to-right traversal of the sub-tree to
     * which the attribute is applied. Accumulate the values produced
     * at the places where `f` is defined into a set and return the
     * set.
     */
    def collects[U] (f : Any ==> U) : CachedAttribute[Any,Set[U]] =
        attr (rwcollects (f))

    /**
     * A decorator that propagates an attribute value down the tree. The
     * partial function `a` should define the value of the attribute at
     * nodes where it is known. If `a` does not define a value for the
     * attribute at a particular node, then the decorator asks the parent
     * of the node for its value of the attribute and uses that value.
     * For this reason, `a` should at least provide a value for the root
     * of the tree.
     */
    def down[T <: Attributable,U] (a : T ==> U) : CachedAttribute[T,U] = {
        lazy val dattr : CachedAttribute[T,U] =
            attr (
                t =>
                    if (a.isDefinedAt (t))
                        a (t)
                    else
                        dattr (t.parent[T])
            )
        dattr
    }

    /**
     * A pair of attributes that thread through a tree in a depth-first
     * left-to-right fashion.  The `in` (`out`) attributes provides the value
     * of the chain as it enters a node from above (leaves a node to the
     * above).
     */
    case class Chain[T,U] (in : (T => U), out : (T => U)) extends (T => U) {
        def apply (t : T) : U = out (t)
    }

    /**
     * An identity function for chain updates. In other words, pass the value
     * of the chain through without making any changes.
     */
    private def idf[T,U] : (T => U) => (T ==> U) =
        f => { case t => f (t) }

    /**
     * Create a new attribute chain.  The `update` functions provide ways to
     * influence the chain value, by taking the default computation of the
     * `in` or `out` attribute and returning a partial function.  If the domain
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
            sys.error (s"chain root of tree reached at $t")
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
