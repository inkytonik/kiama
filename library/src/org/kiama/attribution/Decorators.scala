/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2014 Anthony M Sloane, Macquarie University.
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

import org.kiama.relation.Tree

/**
 * Decorators are higher-order operations that provide common patterns of
 * tree attribution based on simple attributes or functions. A `Tree` must
 * be supplied to give the decorators access to the tree structure.
 */
class Decorators[T <: Product,U <: T] (tree : Tree[T,U]) {

    import org.kiama.attribution.Attribution._
    import scala.PartialFunction

    /**
     * A decorator that progagates an attribute value down the tree. The
     * partial function `a` should define the value of the attribute at
     * the root. The value defined at the root will also be made available
     * at all other nodes.
     */
    def atRoot[U] (a : T => U) : CachedAttribute[T,U] =
        down[U] (a) (PartialFunction.empty)

    /**
     * A decorator that propagates an attribute value down the tree. The
     * partial function `a` should define the value of the attribute at
     * nodes where it is known. If `a` does not define a value for the
     * attribute at a particular node, then the decorator asks the parent
     * of the node for its value of the attribute and uses that value.
     * If no node on the path to the root defines a value for the attribute,
     * then default applied to the root is returned.
     */
    def down[U] (default : T => U) (a : T ==> U) : CachedAttribute[T,U] = {
        lazy val dattr : CachedAttribute[T,U] =
            attr {
                case t if a.isDefinedAt (t) =>
                    a (t)
                case tree.parent (p) =>
                    dattr (p)
                case t =>
                    default (t)
            }
        dattr
    }

    /**
     * Variant of `down` that takes a default value instead of a default function.
     */
    def down[U] (default : U) (a : T ==> U) : CachedAttribute[T,U] =
        down[U] ((_ : T) => default) (a)

    /**
     * Variant of `down` that throws an error if `a` is not defined on the
     * path to the root of the tree.
     */
    def downErr[U] (a : T ==> U) : CachedAttribute[T,U] =
        down[U] ((_ : T) => sys.error ("downErr: function is not defined on path to root")) (a)

    /**
     * Variant of `down` that returns `None` if `a` is not defined on the
     * path to the root of the tree, otherwise it wraps the value that `a`
     * returns in `Some`.
     */
    def downOpt[U] (a : T ==> U) : CachedAttribute[T,Option[U]] =
        down[Option[U]] (None) (a andThen (Some (_)))

    /**
     * A pair of attributes that thread through a tree in a depth-first
     * left-to-right fashion.  The `in` (`out`) attributes provides the value
     * of the chain as it enters a node from above (leaves a node to the
     * above).
     */
    case class Chain[U] (in : (T => U), out : (T => U)) extends (T => U) {
        def apply (t : T) : U = out (t)
    }

    /**
     * An identity function for chain updates. In other words, pass the value
     * of the chain through without making any changes.
     */
    private def idf[U] : (T => U) => (T ==> U) =
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
    def chain[U] (
                 inupdate : (T => U) => (T ==> U) = idf[U],
                 outupdate : (T => U) => (T ==> U) = idf[U]
             ) : Chain[U] = {

        def error (t : T) : Nothing = {
            in.reset
            out.reset
            sys.error (s"chain root of tree reached at $t")
        }

        def indflt (t : T) : U =
            t match {
                case tree.prev (p) =>
                    out (p)
                case tree.parent (p) =>
                    in (p)
                case _ =>
                    error (t)
            }

        lazy val infunc = inupdate (indflt)

        lazy val in : CachedAttribute[T,U] =
            attr {
                case t if infunc.isDefinedAt (t) =>
                    infunc (t)
                case t =>
                    indflt (t)
            }

        def outdflt (t : T) : U =
            t match {
                case tree.lastChild (c) =>
                    out (c)
                case _ =>
                    in (t)
            }

        lazy val outfunc = outupdate (outdflt)

        lazy val out : CachedAttribute[T,U] =
            attr {
                case t if outfunc.isDefinedAt (t) =>
                    outfunc (t)
                case t =>
                    outdflt (t)
            }

        Chain (in, out)
    }

}
