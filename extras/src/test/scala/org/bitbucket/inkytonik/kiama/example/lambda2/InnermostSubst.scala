/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

/**
 * Evaluation of lambda calculus using a memoising innermost evaluation
 * with term-level substitution and arithmetic operations.
 */
trait InnermostSubst extends ReduceSubst {

    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Version of innermost library strategy that memoises.
     */
    def innermost(s : => Strategy) : Strategy =
        memo(all(innermost(s) <* attempt(s <* innermost(s))))

    /**
     * Evaluate expressions starting with the innermost sub-expressions.
     */
    override lazy val s : Strategy =
        innermost(lambda)

}

class InnermostSubstEvaluator extends ReduceSubst {
    override val reducesinlambdas = true
}

