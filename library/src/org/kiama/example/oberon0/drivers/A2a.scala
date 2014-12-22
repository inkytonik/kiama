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
package drivers

trait A2aPhases extends L3.SyntaxAnalyser
        with L3.source.SourcePrettyPrinter
        with base.FrontEndDriver {

    import base.source.SourceTree.SourceTree

    def artefact : String = "A2a"
    def langlevel : Int = 3
    def tasklevel : Int = 2

    def buildAnalyser (atree : SourceTree) : base.Analyser =
        new L3.NameAnalyser {
            val tree = atree
        }

}

object A2a extends A2aPhases
