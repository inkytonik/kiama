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
 * Lazy evaluation of lambda calculus with parallel term-level substitution
 * and arithmetic operations, plus sharing of substituted terms.
 */
trait ParLazyShare extends ParLazy {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Lazily evaluate within the expression then try to reduce the
     * expression itself, repeating until no change.
     */
    override lazy val s : Strategy = {
        lazy val e : Strategy =
            attempt(letAppL(e) + letOpn(e)) <*
                attempt((subsVar + Letp(id, beta + arithop) + letLetRen) <* e)
        letLift <* e <* letDrop
    }

}

class ParLazyShareEvaluator extends ParLazyShare
