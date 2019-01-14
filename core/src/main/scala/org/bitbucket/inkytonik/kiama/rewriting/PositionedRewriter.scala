/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package rewriting

/**
 * Strategy-based term rewriting that copies positions to rewritten terms.
 * The positions are stored in a Kiama `Positions` object.
 *
 * Specifically, this kind of rewriter will record positions of nodes
 * when they are (a) rewritten as part of a generic traversal (e.g.,
 * `all`), or (b) rewritten as part of a `rule` or similar (e.g., `rulefs`).
 *
 * In each case both the start and finish positions of the old node are
 * copied across to the new node into which it is rewritten. In case (b)
 * no attempt is made to assign positions to nodes that represent sub-terms
 * of the term that results from a successful application of the rule.
 * Override the `rewriting` method to add more specific behaviour.
 */
trait PositionedRewriter extends CallbackRewriter {

    import org.bitbucket.inkytonik.kiama.util.Positions

    /**
     * The position store to use for this rewriter.
     */
    val positions = new Positions

    /**
     * Use the `Positioned` support to set the start and finish positions
     * of the new term to be those of the old term. Always return the new
     * term.
     */
    def rewriting[T](oldTerm : T, newTerm : T) : T = {
        positions.dupPos(oldTerm, newTerm)
        newTerm
    }

}

/**
 * Strategy-based term rewriting for Kiama `Positioned` terms.
 */
object PositionedRewriter extends PositionedRewriter
