/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
 * Any-rewriting strategies. A strategy is a function that takes a term
 * of any type as input and either succeeds producing a new term (`Some`),
 * or fails (`None`).
 */
abstract class Strategy extends (Any => Option[Any]) {

    /**
     * Alias this strategy as `p` to make it easier to refer to in the
     * combinator definitions.
     */
    p =>

    import scala.language.experimental.macros

    /**
     * The name of this strategy.
     */
    def name : String

    /**
     * Apply this strategy to a term, producing either a transformed term
     * wrapped in `Some`, or `None`, representing a rewriting failure.
     */
    def apply (r : Any) : Option[Any]

    /**
     * Sequential composition. Construct a strategy that first applies
     * this strategy. If it succeeds, then apply `q` to the new subject
     * term. Otherwise fail.
     */
    def <* (q : Strategy) : Strategy =
        macro RewriterMacros.seqMacro

    /**
     * Builder for `<*`.
     */
    def <* (n : String, q : => Strategy) : Strategy =
        new Strategy {
            val name = n
            def apply (t1 : Any) : Option[Any] =
                p (t1) match {
                    case Some (t2) => q (t2)
                    case None      => None
                }
        }

    /**
     * Deterministic choice.  Construct a strategy that first applies
     * this strategy. If it succeeds, succeed with the resulting term.
     * Otherwise, apply `q` to the original subject te`rm.
     */
    def <+ (q : Strategy) : Strategy =
        macro RewriterMacros.detchoiceMacro

    /**
     * Builder for `<+`.
     */
    def <+ (n : String, q : => Strategy) : Strategy =
        new Strategy {
            val name = n
            def apply (t1 : Any) : Option[Any] =
                p (t1) match {
                    case Some (t2) => Some (t2)
                    case None      => q (t1)
                }
        }

    /**
     * Non-deterministic choice. Normally, construct a strategy that
     * first applies either this strategy or the given strategy. If it
     * succeeds, succeed with the resulting term. Otherwise, apply `q`.
     * Currently implemented as deterministic choice, but this behaviour
     * should not be relied upon.
     * When used as the argument to the `<` conditional choice
     * combinator, `+` just serves to hold the two strategies that are
     * chosen between by the conditional choice.
     */
    def + (q : Strategy) : PlusStrategy =
        macro RewriterMacros.nondetchoiceMacro

    /**
     * Builder for `+`.
     */
    def + (n : String, q : => Strategy) : PlusStrategy =
        new PlusStrategy (n, p, q)

    /**
     * Conditional choice: `c < l + r`. Construct a strategy that first
     * applies this strategy (`c`). If `c` succeeds, the strategy applies
     * `l` to the resulting term, otherwise it applies `r` to the original
     * subject term.
     */
    def < (lr : PlusStrategy) : Strategy =
        macro RewriterMacros.condMacro

    /**
     * Builder for `<`.
     */
    def < (n : String, lr : => PlusStrategy) : Strategy =
        new Strategy {
            val name = n
            def apply (t1 : Any) : Option[Any] =
                p (t1) match {
                    case Some (t2) => lr.left (t2)
                    case None      => lr.right (t1)
                }
        }

}

/**
 * Helper class to contain commonality of choice in non-deterministic
 * choice operator and then-else part of a conditional choice. Only
 * returned by the non-deterministic choice operator.
 */
class PlusStrategy (n : String, p : => Strategy, q : => Strategy) extends Strategy {
    val name = n
    def left = p
    def right = q
    def apply (t : Any) : Option[Any] =
        (p <+ (n, q)) (t)
}
