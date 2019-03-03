/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package base.source

import org.bitbucket.inkytonik.kiama.output.ParenPrettyPrinter

/**
 * Interface for all source pretty-printers.
 */
trait SourcePrettyPrinter extends ParenPrettyPrinter {

    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

    def format(m : ModuleDecl) : Document =
        pretty(toDoc(m))

    def declsection(d : Declaration) : String =
        ""

    def toDoc(n : SourceNode) : Doc =
        n match {
            case ModuleDecl(IdnDef(i1), Block(Vector(), Vector()), IdnUse(i2)) =>
                "MODULE" <+> i1 <> semi <@> "END" <+> i2 <> dot

            case ModuleDecl(IdnDef(i1), b, IdnUse(i2)) =>
                "MODULE" <+> i1 <> semi <@> blockToDoc(b, true) <+> i2 <> dot

            case b : Block =>
                blockToDoc(b)

            case _ =>
                emptyDoc
        }

    /**
     * Pretty-print a block, omitting the BEGIN if there are no statements.
     * No declarations can be present at this level.  Second parameter says
     * whether the BEGIN-END should be included if there are no declarations.
     */
    def blockToDoc(b : Block, beginend : Boolean = false) : Doc =
        b.stmts match {
            case Vector() =>
                "END"
            case ss =>
                if (beginend)
                    "BEGIN" <> semisep(ss) <@> "END"
                else
                    vsep(ss map toDoc, semi)
        }

    /**
     * Pretty-print a nested list of nodes separated by sep (default: semi
     * colon) and line breaks.
     */
    def semisep(l : Vector[SourceNode], sep : Doc = semi) : Doc =
        nest(lsep(l map toDoc, sep))

}
