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

trait A1Phases extends L2.source.SourcePrettyPrinter
    with base.FrontEndDriver {

    import base.source.SourceTree.SourceTree

    def artefact : String = "A1"
    def langlevel : Int = 2
    def tasklevel : Int = 2

    val parsers = new L2.SyntaxAnalyser(positions)
    val parser = parsers.moduledecl

    def buildAnalyser(atree : SourceTree) : base.Analyser =
        new L2.NameAnalyser {
            val tree = atree
        }

}

object A1 extends A1Phases
