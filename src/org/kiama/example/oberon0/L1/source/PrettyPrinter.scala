/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package L1.source

trait PrettyPrinter extends L0.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.{Block, SourceASTNode}
    import L0.source.Expression

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case s : IfStatement =>
                ifToDoc (s)

            case s : WhileStatement =>
                "WHILE" <+> toDoc (s.cond) <+> "DO" <> semisep (s.block.stmts) <@> "END"

            case _ =>
                super.toDoc (n)
        }

    def ifToDoc (s : IfStatement) : Doc = {

        def elsifToDoc (ei : (Expression, Block)) : Doc =
            line <> "ELSIF" <+> toDoc (ei._1) <+> "THEN" <> semisep (ei._2.stmts)

        "IF" <+> toDoc (s.cond) <+> "THEN" <>
        semisep (s.block.stmts) <>
        hcat (s.elsifs map elsifToDoc) <>
        s.optelse.map (b => line <> "ELSE" <> semisep (b.stmts)).getOrElse (empty) <@>
        "END"
    }

}
