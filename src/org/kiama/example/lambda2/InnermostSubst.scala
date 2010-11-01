/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
package example.lambda2

/**
 * Evaluation of lambda calculus using a memoising innermost evaluation
 * with term-level substitution and arithmetic operations.
 */
trait InnermostSubst extends ReduceSubst {

    import AST._
    import org.kiama.rewriting.Rewriter._

    /**
     * Version of innermost library strategy that memoises.
     */
    def innermost (s : => Strategy) : Strategy =
        memo (all (innermost (s) <* attempt (s <* innermost (s))))

    /**
     * Evaluate expressions starting with the innermost sub-expressions.
     */
    override lazy val s : Strategy =
        innermost (lambda)

}

class InnermostSubstEvaluator extends ReduceSubst {
    override def reducesinlambdas () = true
}

