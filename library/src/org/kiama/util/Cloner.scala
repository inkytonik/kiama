/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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
package rewriting

/**
 * Implementation of cloning operations. These operations must be mixed in
 * to an instance of the rewriting library which will then be used as the
 * underlying rewriting behaviour.
 */
trait Cloner {

    self : Rewriter =>

    import org.kiama.relation.Tree.isLeaf
    import org.kiama.util.Memoiser

    /**
     * Deep clone the term `t`. Only applicable if the base type of the tree is
     * a `Product`.
     */
    def deepclone[T <: Product] (t : T) : T = {

        val deepcloner =
            everywherebu (rule[T] {
                case n if isLeaf (n) =>
                    copy (n)
            })

        rewrite (deepcloner) (t)

    }

    /**
     * Lazily deep clone the term `t`; i.e., only clone sub-trees if they occur
     * elsewhere in the tree. Only applicable if the base type of the tree is a
     * `Product`.
     */
    def lazyclone[T <: Product] (t : T) : T = {

        object LeafCache extends Memoiser {
            val seen = new IdMemoised[T,Boolean] {}
        }

        import LeafCache.seen

        val lazycloner =
            everywherebu (rule[T] {
                case n if isLeaf (n) =>
                    if (seen.getWithDefault (n, false))
                        copy (n)
                    else {
                        seen.put (n, true)
                        n
                    }
            })

        rewrite (lazycloner) (t)

    }

}

/**
 * A cloner that uses non-memoising term rewriting.
 */
object Cloner extends Rewriter with Cloner
