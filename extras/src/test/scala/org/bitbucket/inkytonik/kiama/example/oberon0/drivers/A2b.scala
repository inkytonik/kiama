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

trait A2bPhases extends L2.source.SourcePrettyPrinter
    with base.FrontEndDriver {

    import base.source.SourceTree.SourceTree

    def artefact : String = "A2b"
    def langlevel : Int = 2
    def tasklevel : Int = 3

    val parsers = new L2.SyntaxAnalyser(positions)
    val parser = parsers.moduledecl

    def buildAnalyser(atree : SourceTree) : base.Analyser =
        new L2.NameAnalyser with L2.TypeAnalyser {
            val tree = atree
        }

}

object A2b extends A2bPhases
