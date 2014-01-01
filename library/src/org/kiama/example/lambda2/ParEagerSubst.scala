/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
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
 * Eager evaluation of lambda calculus with parallel term-level substitution
 * and arithmetic operations.
 */
trait ParEagerSubst extends Par {

    import LambdaTree._
    import org.kiama.rewriting.Rewriter._
    import org.kiama.rewriting.Strategy

    /**
     * Eagerly evaluate within the expression then try to reduce the
     * expression itself, repeating until no change.
     */
    override lazy val s = {
        lazy val e : Strategy =
            attempt (App (e, e) + Letp (all (Bind (id, e)), id) +
                     Opn (e, id, e)) <* f
        lazy val y : Strategy =
            attempt (App (y, y) + Opn (y, id, y)) <* f
        lazy val f =
            attempt (lambda <* y)
        e
    }

}

class ParEagerSubstEvaluator extends ParEagerSubst
