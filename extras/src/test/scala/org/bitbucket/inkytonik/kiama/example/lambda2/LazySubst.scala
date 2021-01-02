/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

/**
 * Evaluation of lambda calculus using lazy evaluation with
 * term-level substitution and arithmetic operations.
 */
trait LazySubst extends EagerSubst {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Evaluate applied functions, scoped expressions and operands, then
     * try to reduce the expression itself, repeating until no change.
     */
    override lazy val s : Strategy =
        attempt(App(s, id) + Let(id, id, id, s) + Opn(s, id, s)) <*
            attempt(lambda <* s)

}

class LazySubstEvaluator extends LazySubst
