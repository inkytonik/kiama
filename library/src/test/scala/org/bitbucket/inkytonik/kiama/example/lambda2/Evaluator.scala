/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

import LambdaTree._

/**
 * Interface for a lambda2 evaluator.
 */
trait Evaluator {

    import org.bitbucket.inkytonik.kiama.util.Counter

    /**
     * Evaluate the given expression, returning the result of the
     * evaluation if it succeeded, or exp if it failed.
     */
    def eval(exp : Exp) : Exp

    /**
     * Whether this mechanism evaluates inside lambdas.  Used for
     * testing.  Default: false.
     */
    def reducesinlambdas : Boolean =
        false

    /**
     * Counter used to generate fresh names.
     */
    val freshVarCounter = new Counter

    /**
     * Generate a fresh variable name.  Prefix the name with an underscore
     * to avoid the potential for clashes with user-level variables (which
     * must start with a letter).
     */
    def freshVar() : Idn = {
        val count = freshVarCounter.next()
        s"_v$count"
    }

    /**
     * Capture-free substitution of free occurrences of x in e1 with e2.
     */
    def substitute(x : Idn, e2 : Exp, e1 : Exp) : Exp =
        e1 match {
            case Var(y) if x == y =>
                e2
            case Lam(y, t, e3) =>
                val z = freshVar()
                Lam(z, t, substitute(x, e2, substitute(y, Var(z), e3)))
            case App(l, r) =>
                App(substitute(x, e2, l), substitute(x, e2, r))
            case Opn(l, op, r) =>
                Opn(substitute(x, e2, l), op, substitute(x, e2, r))
            case e =>
                e
        }

}

/**
 * Interface for an individual rewriting-based lambda2 evaluator.
 */
trait RewritingEvaluator extends Evaluator {

    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.rewrite
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Evaluate the given expression by rewriting it with the s
     * strategy.
     */
    def eval(exp : Exp) : Exp =
        rewrite(s)(exp)

    /**
     * The strategy to use to perform the evaluation.
     */
    def s : Strategy

}
