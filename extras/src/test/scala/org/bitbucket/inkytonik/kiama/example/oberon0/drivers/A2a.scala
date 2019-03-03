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
package drivers

trait A2aPhases extends L3.source.SourcePrettyPrinter
    with base.FrontEndDriver {

    import base.source.ModuleDecl
    import base.source.SourceTree.SourceTree
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.Source

    def artefact : String = "A2a"
    def langlevel : Int = 3
    def tasklevel : Int = 2

    def parse(source : Source) : ParseResult[ModuleDecl] = {
        val parsers = new L3.SyntaxAnalyser(positions)
        parsers.parseAll(parsers.moduledecl, source)
    }

    def buildAnalyser(atree : SourceTree) : base.Analyser =
        new L3.NameAnalyser {
            val tree = atree
        }

}

object A2a extends A2aPhases
