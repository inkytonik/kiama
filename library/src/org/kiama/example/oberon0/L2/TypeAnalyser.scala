/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package example.oberon0
package L2

trait TypeAnalyser extends L1.TypeAnalyser {

    import base.source.{Expression, IdnUse, SourceTree}
    import L0.source.IdnExp
    import org.kiama.util.Messaging.{check, message, Messages}
    import source.{CaseStatement, ForStatement}

    /**
     * The error checking for this level.
     */
    override def errorsDef (n : SourceTree) : Messages =
        super.errorsDef (n) ++
        check (n) {
            case ForStatement (IdnExp (u @ IdnUse (i)), _, _, _, _) if !isVariable (u->entity) =>
                message (u, s"illegal FOR loop control variable $i")
        }

    override def exptypeDef : Expression => Type =
        (n =>
            n.parent match {
                case _ : ForStatement | _ : CaseStatement =>
                    integerType

                case _ =>
                    super.exptypeDef (n)
            })

}
