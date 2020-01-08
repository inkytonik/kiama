/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L1

trait TypeAnalyser extends L0.TypeAnalyser {

    import base.source.Expression
    import source.{IfStatement, WhileStatement}

    /**
     * The type expected of an expression as defined by its context.
     */
    override def exptypeDef : Expression => Type =
        {
            case tree.parent(_ : IfStatement | _ : WhileStatement) =>
                booleanType

            case n =>
                super.exptypeDef(n)
        }

}
