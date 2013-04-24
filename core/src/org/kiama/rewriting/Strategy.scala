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
 * or fails (`None`). `name` is used to identify this strategy in debugging
 * output.
 */
abstract class Strategy (val name : String) extends (Any => Option[Any]) {

    /**
     * Alias this strategy as `p` to make it easier to refer to in the
     * combinator definitions.
     */
    p =>

    import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}
    import scala.language.experimental.macros

    /**
     * Make one of these strategies with the given name and body `f`.
     */
    def mkStrategy (name : String, f : Any => Option[Any]) : Strategy =
        new Strategy (name) {
            val body = f
        }

    /**
     * Implementation of this strategy. When applied to a term produce either
     * a transformed term wrapped in `Some`, or `None`, representing a
     * rewriting failure.
     */
    val body : Any => Option[Any]

    /**
     * Apply this strategy to a term. By default, just run the implementation
     * body wrapped in profiling.
     */
    def apply (r : Any) : Option[Any] = {
        val i = start ("event" -> "StratEval", "strategy" -> this,
                       "subject" -> r, "subjectHash" -> r.##)
        val result = body (r)
        finish (i, "result" -> result)
        result
    }

    /**
     * Sequential composition. Construct a strategy that first applies
     * this strategy. If it succeeds, then apply `q` to the new subject
     * term. Otherwise fail. `q` is evaluated at most once.
     */
    def <* (q : Strategy) : Strategy =
        macro RewriterCoreMacros.seqMacro

    /**
     * As for the other `<*` with the first argument specifying a name for
     * the constructed strategy.
     */
    def <* (name : String, q : => Strategy) : Strategy =
        mkStrategy (name,
            t1 =>
                p (t1) match {
                    case Some (t2) => q (t2)
                    case None      => None
                }
        )

    /**
     * Deterministic choice.  Construct a strategy that first applies
     * this strategy. If it succeeds, succeed with the resulting term.
     * Otherwise, apply `q` to the original subject term. `q` is
     * evaluated at most once.
     */
    def <+ (q : Strategy) : Strategy =
        macro RewriterCoreMacros.detchoiceMacro

    /**
     * As for the other `<+` with the first argument specifying a name for
     * the constructed strategy.
     */
    def <+ (name : String, q : => Strategy) : Strategy =
        mkStrategy (name,
            (t1 : Any) =>
                p (t1) match {
                    case Some (t2) => Some (t2)
                    case None      => q (t1)
                }
        )

    /**
     * Non-deterministic choice. Normally, construct a strategy that
     * first applies either this strategy or the given strategy. If it
     * succeeds, succeed with the resulting term. Otherwise, apply `q`.
     * Currently implemented as deterministic choice, but this behaviour
     * should not be relied upon.
     * When used as the argument to the `<` conditional choice
     * combinator, `+` just serves to hold the two strategies that are
     * chosen between by the conditional choice.
     * `q` is evaluated at most once.
     */
    def + (q : Strategy) : PlusStrategy =
        macro RewriterCoreMacros.nondetchoiceMacro

    /**
     * As for the other `+` with the first argument specifying a name for
     * the constructed strategy.
     */
    def + (name : String, q : => Strategy) : PlusStrategy =
        new PlusStrategy (name, p, q)

    /**
     * Conditional choice: `c < l + r`. Construct a strategy that first
     * applies this strategy (`c`). If `c` succeeds, the strategy applies
     * `l` to the resulting term, otherwise it applies `r` to the original
     * subject term. `lr` is evaluated at most once.
     */
    def < (lr : PlusStrategy) : Strategy =
        macro RewriterCoreMacros.condMacro

    /**
     * As for the other `<` with the first argument specifying a name for
     * the constructed strategy.
     */
    def < (name : String, lr : => PlusStrategy) : Strategy =
        mkStrategy (name,
            t1 =>
                p (t1) match {
                    case Some (t2) => lr.left (t2)
                    case None      => lr.right (t1)
                }
        )

    /**
     * Identify this strategy by its name.
     */
    override def toString : String =
        name

}
