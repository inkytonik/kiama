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

import org.kiama.util.Memoiser

/**
 * Strategy-based term rewriting where all strategy results are memoised
 * by identity on the subject term.
 */
trait MemoRewriter extends Rewriter with Memoiser {

    import org.bitbucket.inkytonik.dsprofile.Events.{finish, start}
    import scala.collection.immutable.Seq

    /*
     * Any-rewriting strategies that memoise their results by identity on
     * the subject term.
     */
    abstract class MemoStrategy (name : String) extends
            Strategy (name) with IdMemoised[Any,Option[Any]] {

        /**
         * Make one of these strategies with the given name and body `f`.
         */
        override def mkStrategy (name : String, f : Any => Option[Any]) : Strategy =
            new MemoStrategy (name) {
                val body = f
            }

        /**
         * Return the value of this attribute for node `t`, raising an error if
         * it depends on itself.
         */
        override def apply (r : Any) : Option[Any] = {
            val i = start (Seq ("event" -> "StratEval", "strategy" -> this,
                                "subject" -> r, "subjectHash" -> r.##))
            get (r) match {
                case None =>
                    val u = body (r)
                    put (r, u)
                    finish (i, Seq ("cached" -> false, "result" -> u))
                    u
                case Some (u) =>
                    finish (i, Seq ("cached" -> true, "result" -> u))
                    u
            }
        }

    }

    /**
     * Make a memoising strategy with the given name and body `f`.
     */
    override def mkStrategy (name : String, f : Any => Option[Any]) : Strategy =
        new MemoStrategy (name) {
            val body = f
        }

    /**
     * Build a memoising strategy. Since all strategies here are memoised, this
     * method is the identity.
     */
    override def memo (name : String, s : => Strategy) : Strategy =
        s

}

/**
 * Memoising strategy-based term rewriting for arbitrary terms.
 */
object MemoRewriter extends MemoRewriter
