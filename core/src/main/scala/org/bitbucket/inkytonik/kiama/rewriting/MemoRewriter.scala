/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package rewriting

/**
 * Strategy-based term rewriting where all strategy results are memoised
 * by identity on the subject term.
 */
trait MemoRewriter extends Rewriter {

    import org.bitbucket.inkytonik.kiama.util.Memoiser.makeIdMemoiser

    /*
     * Any-rewriting strategies that memoise their results by identity on
     * the subject term.
     */
    abstract class MemoStrategy extends Strategy {

        /**
         * Backing memo table.
         */
        val memo = makeIdMemoiser[Any, Option[Any]]()

        /**
         * Make one of these strategies with the body `f`.
         */
        override def mkStrategy(f : Any => Option[Any]) : Strategy =
            new MemoStrategy {
                override def apply(t : Any) =
                    f(t)
            }

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        override def apply(r : Any) : Option[Any] = {
            memo.get(r) match {
                case None =>
                    val u = apply(r)
                    memo.put(r, u)
                    u
                case Some(u) =>
                    u
            }
        }

        /**
         * Has this strategy been computed on `r`?
         */
        def hasBeenComputedAt(r : Any) : Boolean =
            memo.hasBeenComputedAt(r)

        /**
         * Reset the memo table that backs this strategy.
         */
        def reset() : Unit = {
            memo.reset()
        }

    }

    /**
     * Make a memoising strategy with the body `f`.
     */
    override def mkStrategy(f : Any => Option[Any]) : Strategy =
        new MemoStrategy {
            override def apply(t : Any) =
                f(t)
        }

    /**
     * Build a memoising strategy. Since all strategies here are memoised, this
     * method is the identity.
     */
    override def memo(s : Strategy) : Strategy =
        s

}

/**
 * Memoising strategy-based term rewriting for arbitrary terms.
 */
object MemoRewriter extends MemoRewriter
