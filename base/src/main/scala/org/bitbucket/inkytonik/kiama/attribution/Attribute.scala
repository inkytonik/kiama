/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

/**
 * Common functionality for all attributes.
 */
abstract class Attribute[T, U](val name : String) extends (T => U) {

    /**
     * Report a cycle in the calculation of this attribute discovered when
     * evaluating the attribute on value `t`. Throws an `IllegalStateException`.
     */
    def reportCycle(t : T) : U =
        throw new IllegalStateException(s"Cycle detected in attribute evaluation '$name' at $t")

    /**
     * The attribute's string representation is its name.
     */
    override def toString : String = name

}
