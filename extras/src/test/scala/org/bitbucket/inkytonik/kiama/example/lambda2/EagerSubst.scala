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
 * Eager evaluation of lambda calculus with term-level substitution and
 * arithmetic operations.
 */
trait EagerSubst extends ReduceSubst {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Evaluate applications, local bindings and operands, then try to
     * reduce the expression itself, repeating until no change.
     */
    override lazy val s : Strategy =
        attempt(App(s, s) + Let(id, id, s, s) + Opn(s, id, s)) <*
            attempt(lambda <* s)

}

class EagerSubstEvaluator extends EagerSubst
