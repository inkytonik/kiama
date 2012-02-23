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
 * Strategy-based term rewriting for terms with positions.
 *
 * Specifically, this kind of rewriter will preserve positions of nodes
 * that are instances of `scala.util.parsing.input.Positional`, when 
 * they are (a) rewrittten as part of a generic traversal (e.g., `all`),
 * or (b) rewritten as part of a `rule` or similar (e.g., `rulefs`).
 *
 * In each case the position of the old node is copied across to the
 * new node into which it is rewritten. In case (b) no attempt is made
 * to assign positions to nodes that represent sub-terms of the term
 * that results from a successful application of the rule. Override the
 * `rewriting` method to add more specific behaviour.
 */
class PositionalRewriter extends CallbackRewriter {

    import scala.util.parsing.input.Positional

    /**
     * If the two terms are instances of `scala.util.parsing.input.Positional`,
     * set the position of the new term to be that of the old term. Always
     * return the new term.
     */    
    def rewriting[T <: Term] (oldTerm : T, newTerm : T) : T = {
        (oldTerm, newTerm) match {
            case (o : Positional, n : Positional) =>
                n.setPos (o.pos)
            case _ =>
                // Do nothing
        }
        newTerm
    }

}

/**
 * Strategy-based term rewriting for terms with positions.
 */
object PositionalRewriter extends PositionalRewriter
