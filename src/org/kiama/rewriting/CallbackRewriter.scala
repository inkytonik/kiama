/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
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
 */
abstract class CallbackRewriter extends Rewriter {

    /**
     * The method to call when a rewrite operation has happened. It will
     * be called under two circumstances. First, when a rule (or similar, such
     * as rulefs, or strategy) is about to return a new term to replace an old
     * term. (Note that if the rule creates sub-terms in the new term, the
     * results of these operations are not notified, only the root of the
     * new term.) Second, whenever a generic traversal (such as all or one)
     * creates a new node to duplicate an old one. In both cases this method
     * is called with both the old and the new terms. The return value should
     * be a term that should go forward as the new term.
     */
    def rewriting[T <: Term] (oldTerm : T, newTerm : T) : T

    /**
     * Produce a strategy that first runs the strategy s on the current term. 
     * If s fails, then fail. Otherwise, pass the original and new terms to
     * the rewriting method and succeed with the term that it returns.
     */
    private def dispatch (s : Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                s (t) match {
                    case None     => None
                    case Some (n) => Some (rewriting (t, n))
                }
            }
        }

    /**
     * Make a callback-enabled strategy from a function.
     */
    override def strategyf (f : Term => Option[Term]) : Strategy =
        dispatch (super.strategyf (f))

    /**
     * Make a callback-enabled strategy from a partial function.
     */
    override def strategy (f : Term ==> Option[Term]) : Strategy =
        dispatch (super.strategy (f))

    /**
     * Define a callback-enabled rewrite rule using a partial function.
     */
    override def rule (f : Term ==> Term) : Strategy =
        dispatch (super.rule (f))

    /**
     * Define a callback-enabled rewrite rule using a function that returns a
     * strategy.
     */
    override def rulefs (f : Term ==> Strategy) : Strategy =
        dispatch (super.rulefs (f))

    /**
     * General product duplication function with callback notification.
     */
    protected override def dup[T <: Product] (t : T, children : Array[AnyRef]) : T =
        rewriting (t, super.dup (t, children))

}
