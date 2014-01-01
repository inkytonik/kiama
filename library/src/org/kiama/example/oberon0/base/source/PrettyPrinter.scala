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
package base.source

trait PrettyPrinter extends SourcePrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import scala.collection.immutable.Seq

    def declsection (d : Declaration) : String =
        ""

    def toDoc (n : SourceTree) : Doc =
        n match {
            case ModuleDecl (IdnDef (i1), Block (Nil, Nil), IdnUse (i2)) =>
                "MODULE" <+> i1 <> semi <@> "END" <+> i2 <> dot

            case ModuleDecl (IdnDef (i1), b, IdnUse (i2)) =>
                "MODULE" <+> i1 <> semi <@> blockToDoc (b, true) <+> i2 <> dot

            case b : Block =>
                blockToDoc (b)

            case _ =>
                empty
        }

    /**
     * Pretty-print a block, omitting the BEGIN if there are no statements.
     * No declarations can be present at this level.  Second parameter says
     * whether the BEGIN-END should be included if there are no declarations.
     */
    def blockToDoc (b : Block, beginend : Boolean = false) : Doc =
        b.stmts match {
            case Nil => "END"
            case ss  =>
                if (beginend)
                    "BEGIN" <> semisep (ss) <@> "END"
                else
                    vsep (ss map toDoc, semi)
        }

    /**
     * Pretty-print a nested list of nodes separated by sep (default: semi
     * colon) and line breaks.
     */
    def semisep (l : Seq[SourceTree], sep : Doc = semi) : Doc =
        nest (lsep (l map toDoc, sep))

}
