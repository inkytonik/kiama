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
 * Evaluation of lambda calculus using global beta reduction with
 * term-level substitution and arithmetic operations.
 */
trait ReduceSubst extends Reduce {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

    /**
     * Evaluate by repeatedly trying to apply beta reduction and arithmetic
     * operators anywhere.
     */
    override lazy val s =
        reduce(lambda)

    /**
     * Reusable strategy for reduction with explicit term-level substitution.
     */
    lazy val lambda =
        beta + arithop + subsNum + subsVar + subsApp + subsLam + subsOpn

    /**
     * Beta reduction via term-level substitution.
     */
    override lazy val beta =
        rule[Exp] {
            case App(Lam(x, t, e1), e2) => Let(x, t, e2, e1)
        }

    /**
     * Substitution in numeric terms.
     */
    lazy val subsNum =
        rule[Exp] {
            case Let(_, _, _, e : Num) => e
        }

    /**
     * Substitution in variable terms.
     */
    lazy val subsVar =
        rule[Exp] {
            case Let(x, _, e, Var(y)) if x == y => e
            case Let(_, _, _, v : Var)          => v
        }

    /**
     * Substitution in applications.
     */
    lazy val subsApp =
        rule[Exp] {
            case Let(x, t, e, App(e1, e2)) =>
                App(Let(x, t, e, e1), Let(x, t, e, e2))
        }

    /**
     * Substitution in lambda abstractions.
     */
    lazy val subsLam =
        rule[Exp] {
            case Let(x, t1, e1, Lam(y, t2, e2)) if x == y =>
                Lam(y, t2, e2)
            case Let(x, t1, e1, Lam(y, t2, e2)) =>
                val z = freshVar()
                Lam(z, t2, Let(x, t1, e1, Let(y, t2, Var(z), e2)))
        }

    /**
     * Substitution in primitive operations
     */
    lazy val subsOpn =
        rule[Exp] {
            case Let(x, t, e1, Opn(e2, op, e3)) =>
                Opn(Let(x, t, e1, e2), op, Let(x, t, e1, e3))
        }

}

class ReduceSubstEvaluator extends ReduceSubst {
    override val reducesinlambdas = true
}

