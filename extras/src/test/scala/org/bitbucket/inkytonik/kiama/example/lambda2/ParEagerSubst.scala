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
 * Eager evaluation of lambda calculus with parallel term-level substitution
 * and arithmetic operations.
 */
trait ParEagerSubst extends Par {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Eagerly evaluate within the expression then try to reduce the
     * expression itself, repeating until no change.
     */
    override lazy val s = {
        lazy val e : Strategy =
            attempt(App(e, e) + Letp(all(Bind(id, e)), id) +
                Opn(e, id, e)) <* f
        lazy val y : Strategy =
            attempt(App(y, y) + Opn(y, id, y)) <* f
        lazy val f =
            attempt(lambda <* y)
        e
    }

}

class ParEagerSubstEvaluator extends ParEagerSubst
