/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.kiama
package rewriting

/**
 * Helper class to contain commonality of choice in non-deterministic
 * choice operator and then-else part of a conditional choice. Only
 * returned by the non-deterministic choice operator. The first argument
 * specifies a name for the constructed strategy. `p` and `q` are
 * evaluated at most once.
 */
class PlusStrategy (name : String, p : => Strategy, q : => Strategy) extends Strategy (name) {

    /**
     * The left alternative of the choice.
     */
    lazy val left = p

    /**
     * The right alternative of the choice.
     */
    lazy val right = q

    /**
     * The strategy itself (lazily computed).
     */
    private lazy val s = left <+ (name, right)

    /**
     * Implementation of this strategy. Just apply `s`.
     */
    val body =
        (t : Any) =>
            s (t)

}
