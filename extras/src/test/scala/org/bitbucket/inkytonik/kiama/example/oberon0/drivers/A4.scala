/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package drivers

trait A4Phases extends L4.source.SourcePrettyPrinter
    with L4.c.CPrettyPrinter
    with base.TranslatingDriver {

    phases =>

    import base.source.ModuleDecl
    import base.source.SourceTree.SourceTree
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.Source

    def artefact : String = "A4"
    def langlevel : Int = 4
    def tasklevel : Int = 6

    def parse(source : Source) : ParseResult[ModuleDecl] = {
        val parsers = new L4.SyntaxAnalyser(positions)
        parsers.parseAll(parsers.moduledecl, source)
    }

    def buildAnalyser(atree : SourceTree) : L0.TypeAnalyser =
        new L4.NameAnalyser with L4.TypeAnalyser {
            val tree = atree
        }

    def buildTransformer(atree : SourceTree) : base.Transformer =
        new L2.Lifter with L2.Desugarer {
            def buildAnalyser(atree : SourceTree) : L0.TypeAnalyser =
                phases.buildAnalyser(atree)
        }

    def buildTranslator(atree : SourceTree) : base.Translator =
        new L4.CCodeGenerator {
            val tree = atree
        }

}

object A4 extends A4Phases
