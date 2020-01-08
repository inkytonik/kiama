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
package L1.source

trait SourcePrettyPrinter extends L0.source.SourcePrettyPrinter {

    import base.source.{Block, Expression, SourceNode}

    override def toDoc(n : SourceNode) : Doc =
        n match {
            case s : IfStatement =>
                ifToDoc(s)

            case s : WhileStatement =>
                "WHILE" <+> toDoc(s.cond) <+> "DO" <> semisep(s.block.stmts) <@> "END"

            case _ =>
                super.toDoc(n)
        }

    def ifToDoc(s : IfStatement) : Doc = {

        def elsifToDoc(ei : (Expression, Block)) : Doc =
            line <> "ELSIF" <+> toDoc(ei._1) <+> "THEN" <> semisep(ei._2.stmts)

        "IF" <+> toDoc(s.cond) <+> "THEN" <>
            semisep(s.block.stmts) <>
            hcat(s.elsifs map elsifToDoc) <>
            s.optelse.map(b => line <> "ELSE" <> semisep(b.stmts)).getOrElse(emptyDoc) <@>
            "END"
    }

}
