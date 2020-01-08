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
package L2

trait TypeAnalyser extends L1.TypeAnalyser {

    import base.source.{Expression, IdnUse, SourceNode}
    import L0.source.IdnExp
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, message, Messages}
    import source.{CaseStatement, ForStatement}

    /**
     * The error checking for this level.
     */
    override def errorsDef(n : SourceNode) : Messages =
        super.errorsDef(n) ++
            check(n) {
                case ForStatement(IdnExp(u @ IdnUse(i)), _, _, _, _) if !isVariable(entity(u)) =>
                    message(u, s"illegal FOR loop control variable $i")
            }

    override def exptypeDef : Expression => Type =
        {
            case tree.parent(_ : ForStatement | _ : CaseStatement) =>
                integerType

            case n =>
                super.exptypeDef(n)
        }

}
