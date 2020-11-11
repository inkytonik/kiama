/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

/**
 * Common support for attribution of syntax trees in a functional style.
 * Includes circular and constant attributes but needs to be augmented with
 * basic attributes and parameterised attributes.
 */
trait AttributionCommon {

    /**
     * A constant attribute of a node type `T` with value of type `U`. The
     * value is given by the computation `u` which is evaluated at most once.
     */
    class ConstantAttribute[T, U](u : => U) extends Attribute[T, U] {

        /**
         * Lazily computed result of evaluating the attribute's computation.
         */
        private lazy val result = u

        /**
         * Return the value of this attribute for node `t`, always returning
         * `u` but only evaluating it once.
         */
        def apply(t : T) : U =
            result

    }

    /**
     * Define a constant attribute of `T` nodes of type `U` given by the value
     * `u`. `u` is evaluated at most once.
     */
    def constant[T, U](u : => U) : Attribute[T, U] =
        new ConstantAttribute[T, U](u)

}
