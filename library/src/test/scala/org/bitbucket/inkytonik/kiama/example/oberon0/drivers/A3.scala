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
package drivers

trait A3Phases extends L3.source.SourcePrettyPrinter
    with base.TransformingDriver {

    phases =>

    import base.source.SourceTree.SourceTree

    def artefact : String = "A3"
    def langlevel : Int = 3
    def tasklevel : Int = 3

    val parsers = new L3.SyntaxAnalyser(positions)
    val parser = parsers.moduledecl

    def buildAnalyser(atree : SourceTree) : L0.TypeAnalyser =
        new L3.NameAnalyser with L3.TypeAnalyser {
            val tree = atree
        }

    def buildTransformer(atree : SourceTree) : base.Transformer =
        new L2.Lifter with L2.Desugarer {
            def buildAnalyser(atree : SourceTree) : L0.TypeAnalyser =
                phases.buildAnalyser(atree)
        }

}

object A3 extends A3Phases
