/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

/**
 * Lazy evaluation of lambda calculus with parallel term-level substitution
 * and arithmetic operations, plus sharing and update of substituted terms.
 */
trait ParLazyUpdate extends ParLazy {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy

    /**
     * Lazily evaluate within the expression then try to reduce the
     * expression itself, repeating until no change.
     */
    override lazy val s = {
        lazy val e : Strategy =
            attempt(letAppL(e) + letOpn(e)) <*
                attempt(update(subsVar <* e) + Letp(id, beta + arithop) +
                    letLetRen <* e)
        letLift <* e <* letDrop
    }

    /**
     * Update variable bindings using a given evaluation strategy.
     */
    def update(eval : Strategy) : Strategy =
        rulefs[Letp] {
            case Letp(ds1, v @ Var(x)) =>
                option(eval(Letp(ds1, v))) <* rule[Letp] {
                    case Letp(ds2, e) => Letp(Bind(x, e) +: ds2, e)
                }
        }

}

class ParLazyUpdateEvaluator extends ParLazyUpdate
