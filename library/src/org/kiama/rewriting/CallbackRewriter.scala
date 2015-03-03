/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2015 Anthony M Sloane, Macquarie University.
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
 * Strategy-based term rewriting with callbacks. Clients can register
 * functions that are called whenever a rewrite operation has happened.
 * See the `Rewriter` class documentation for more detail on the methods
 * defined here.
 */
trait CallbackRewriter extends Rewriter {

    /**
     * The method to call when a rewrite operation has happened. It will
     * be called under two circumstances. First, when a `rule` (or similar, such
     * as `rulefs`, or `strategy`) is about to return a new term to replace an old
     * term. (Note that if the rule creates sub-terms in the new term, the
     * results of these operations are not notified, only the root of the
     * new term.) Second, whenever a generic traversal (such as all or one)
     * creates a new node to duplicate an old one. In both cases this method
     * is called with both the old and the new terms. The return value should
     * be a term that should go forward as the new term.
     */
    def rewriting[T] (oldTerm : T, newTerm : T) : T

    /**
     * Produce a strategy named `n` that first runs the strategy s on the
     * current term. If `s` fails, then fail. Otherwise, pass the original
     * and new terms to the rewriting method and succeed with the term that
     * it returns.
     */
    def dispatch (name : String, s : Strategy) : Strategy =
        new Strategy (name) {
            val body =
                (t : Any) =>
                    s (t) match {
                        case None     => None
                        case Some (u) => Some (rewriting (t, u))
                    }
        }

    override def ruleWithName[T] (n : String, f : T ==> T) : Strategy =
        dispatch (n, super.ruleWithName[T] (n, f))

    override def rulef (n : String, f : Any => Any) : Strategy =
        dispatch (n, super.rulef (n, f))

    override def rulefsWithName[T] (n : String, f : T ==> Strategy) : Strategy =
        dispatch (n, super.rulefsWithName[T] (n, f))

    override def strategyWithName[T] (n : String, f : T ==> Option[T]) : Strategy =
        dispatch (n, super.strategyWithName (n, f))

    override def strategyf (n : String, f : Any => Option[Any]) : Strategy =
        dispatch (n, super.strategyf (n, f))

    /**
     * Product duplication with callback notification.
     */
    override def dup[T <: Product] (t : T, children : Array[AnyRef]) : T =
        rewriting (t, super.dup (t, children))

}
