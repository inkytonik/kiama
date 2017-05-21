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

/**
 * Interface to switchable evaluators for the lambda2 language.  Enables
 * a client to select an evaluation mechanism by name and access the
 * evaluator for that mechanism.
 */
object Evaluators {

    /**
     * Map of evaluator names to the evaluators themselves.
     * Comments refer to names in Dolstra and Visser paper.
     */
    val evalmap =
        Map(
            "reduce" -> new ReduceEvaluator, // eval1
            "reducesubst" -> new ReduceSubstEvaluator, // eval2
            "innermostsubst" -> new InnermostSubstEvaluator, // eval3
            "eagersubst" -> new EagerSubstEvaluator, // eval4, eval5
            "lazysubst" -> new LazySubstEvaluator, // eval6
            "pareagersubst" -> new ParEagerSubstEvaluator, // eval7
            "parlazysubst" -> new ParLazySubstEvaluator, // eval8
            "parlazyshare" -> new ParLazyShareEvaluator, // eval9
            "parlazyupdate" -> new ParLazyUpdateEvaluator
        ) // eval10

    /**
     * Return the names of the available evaluation mechanisms.
     */
    val mechanisms = evalmap.keySet

    /**
     * The evaluator for the given mechanism.
     */
    def evaluatorFor(mechanism : String) : Evaluator =
        evalmap(mechanism)

}
