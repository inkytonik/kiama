/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

/**
 * Evaluation of lambda calculus using global beta reduction with meta-level
 * substitution and arithmetic operations.
 */
trait Reduce extends RewritingEvaluator {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

    /**
     * Evaluate by repeatedly trying to apply beta reduction and arithmetic
     * operators anywhere.
     */
    lazy val s =
        reduce(beta + arithop)

    /**
     * Beta reduction via meta-level substitution.
     */
    lazy val beta =
        rule[Exp] {
            case App(Lam(x, _, e1), e2) => substitute(x, e2, e1)
        }

    /*
     * Evaluation of arithmetic operators.
     */
    lazy val arithop =
        rule[Exp] {
            case Opn(Num(l), op, Num(r)) => Num(op.eval(l, r))
        }

}

class ReduceEvaluator extends Reduce {
    override val reducesinlambdas = true
}
