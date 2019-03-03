/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

import org.bitbucket.inkytonik.kiama.util.Compat210._

object AttributionCoreMacros {

    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName
    import scala.reflect.macros._

    // Avoid unused import warning for Compat210
    val used = dummy

    // Macros for the builder methods

    def attrMacro[T, U, A](c : blackbox.Context)(f : c.Expr[T => U]) : c.Expr[A] =
        makeCallWithName(c, "this.attrWithName")

    def circularMacro[T, U, A](c : blackbox.Context)(init : c.Expr[U])(f : c.Expr[T => U]) : c.Expr[A] =
        makeCallWithName(c, "this.circularWithName")

    def dynAttrMacro[T, U, A](c : blackbox.Context)(f : c.Expr[T => U]) : c.Expr[A] =
        makeCallWithName(c, "this.dynAttrWithName")

    def paramAttrMacro[V, T, U, P](c : blackbox.Context)(f : c.Expr[V => T => U]) : c.Expr[P] =
        makeCallWithName(c, "this.paramAttrWithName")

}
