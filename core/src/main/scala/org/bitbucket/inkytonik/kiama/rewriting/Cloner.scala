/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package rewriting

/**
 * Implementation of cloning operations. These operations must be mixed in
 * to an instance of the rewriting library which will then be used as the
 * underlying rewriting behaviour.
 */
trait Cloner {

    self : Rewriter =>

    import org.bitbucket.inkytonik.kiama.relation.TreeRelation.isLeaf

    /**
     * Deep clone the term `t`. Only applicable if the base type of the tree is
     * a `Product`.
     */
    def deepclone[T <: Product](t : T) : T = {

        val deepcloner =
            everywherebu(rule[T] {
                case n if isLeaf(n) =>
                    copy(n)
            })

        rewrite(deepcloner)(t)

    }

    /**
     * Lazily deep clone the term `t`; i.e., only clone sub-trees if they occur
     * elsewhere in the tree. Only applicable if the base type of the tree is a
     * `Product`. The `bu` argument specifies the strategy to use when traversing
     * the term. It should be a bottom-up traversal, but can be tailored to skip
     * some sub-trees if desired. `bu` defaults to `everywherebu`.
     */
    def lazyclone[T <: Product](
        t : T,
        bu : Strategy => Strategy = everywherebu("everywherebu", _)
    ) : T = {

        import org.bitbucket.inkytonik.kiama.util.Memoiser.makeIdMemoiser

        val seen = makeIdMemoiser[Product, Boolean]()

        val lazycloner =
            bu(rule[T] {
                case n if isLeaf(n) =>
                    if (seen.getOrDefault(n, false))
                        copy(n)
                    else {
                        seen.put(n, true)
                        n
                    }
            })

        rewrite(lazycloner)(t)

    }

}

/**
 * A cloner that uses non-memoising term rewriting.
 */
object Cloner extends Rewriter with Cloner
