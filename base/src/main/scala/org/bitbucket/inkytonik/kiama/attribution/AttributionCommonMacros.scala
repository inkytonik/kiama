/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package attribution

object AttributionCommonMacros {

    import org.bitbucket.inkytonik.dsinfo.DSInfo.makeCallWithName
    import scala.reflect.macros._

    // Macros for the builder methods

    def constantMacro[T, U](c : blackbox.Context)(u : c.Expr[U]) : c.Expr[Attribute[T, U]] =
        makeCallWithName(c)

}
