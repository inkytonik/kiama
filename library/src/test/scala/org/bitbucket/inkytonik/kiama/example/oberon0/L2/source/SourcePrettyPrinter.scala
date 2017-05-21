/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L2.source

trait SourcePrettyPrinter extends L1.source.SourcePrettyPrinter {

    import base.source.SourceNode

    override def toDoc(n : SourceNode) : Doc =
        n match {
            case s : ForStatement =>
                forToDoc(s)

            case s : CaseStatement =>
                caseToDoc(s)

            case _ =>
                super.toDoc(n)
        }

    def forToDoc(s : ForStatement) : Doc =
        "FOR" <+> toDoc(s.idn) <+> ":=" <+> toDoc(s.lower) <+>
            "TO" <+> toDoc(s.upper) <+>
            s.by.map(e => "BY" <+> toDoc(e) <> space).getOrElse(emptyDoc) <>
            "DO" <> semisep(s.block.stmts) <@> "END"

    def caseToDoc(s : CaseStatement) : Doc =
        "CASE" <+> toDoc(s.exp) <+> "OF" <>
            casesToDoc(s.cases) <@>
            s.optelse.map(b => "ELSE" <> semisep(b.stmts) <> line).getOrElse(emptyDoc) <>
            "END"

    def casesToDoc(l : Vector[Case]) : Doc = {

        def condToDoc(cond : Condition) : Doc =
            cond match {
                case ValCond(e) =>
                    toDoc(e)
                case MinMaxCond(min, max) =>
                    toDoc(min) <+> ".." <+> toDoc(max)
            }

        def condsToDoc(conds : Vector[Condition]) : Doc =
            hsep(conds map condToDoc, comma)

        def singleCaseToDoc(kase : Case) : Doc =
            condsToDoc(kase.conds) <+> ":" <+>
                hsep(kase.block.stmts map toDoc, semi)

        nest(line <> "  " <> ssep(l map singleCaseToDoc, line <> "| "), 2)

    }

}
