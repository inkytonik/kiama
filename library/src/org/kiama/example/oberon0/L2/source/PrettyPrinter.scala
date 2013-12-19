/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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
package L2.source


trait PrettyPrinter extends L1.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.SourceTree
    import scala.collection.immutable.Seq

    override def toDoc (n : SourceTree) : Doc =
        n match {
            case s : ForStatement =>
                forToDoc (s)

            case s : CaseStatement =>
                caseToDoc (s)

            case _ =>
                super.toDoc (n)
        }

    def forToDoc (s : ForStatement) : Doc =
        "FOR" <+> toDoc (s.idn) <+> ":=" <+> toDoc (s.lower) <+>
            "TO" <+> toDoc (s.upper) <+>
            s.by.map (e => "BY" <+> toDoc (e) <> space).getOrElse (empty) <>
            "DO" <> semisep (s.block.stmts) <@> "END"

    def caseToDoc (s : CaseStatement) : Doc =
        "CASE" <+> toDoc (s.exp) <+> "OF" <>
        casesToDoc (s.cases) <@>
        s.optelse.map (b => "ELSE" <> semisep (b.stmts) <> line).getOrElse (empty) <>
        "END"

    def casesToDoc (l : Seq[Case]) : Doc = {

        def condToDoc (cond : Condition) : Doc =
            cond match {
                case ValCond (e) =>
                    toDoc (e)
                case MinMaxCond (min, max) =>
                    toDoc (min) <+> ".." <+> toDoc (max)
            }

        def condsToDoc (conds : Seq[Condition]) : Doc =
            hsep (conds map condToDoc, comma)

        def singleCaseToDoc (kase : Case) : Doc =
            condsToDoc (kase.conds) <+> ":" <+>
            hsep (kase.block.stmts map toDoc, semi)

        nest (line <> "  " <> ssep (l map singleCaseToDoc, line <> "| "), 2)

    }

}
