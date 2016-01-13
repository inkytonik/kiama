/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2016 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.prolog

import PrologTree.Program
import org.bitbucket.inkytonik.kiama.util.{Compiler, TestCompiler}

/**
 * Tests that check that the semantic analyser works correctly.  I.e., it correctly
 * diagnoses errors where they are present, and passes correct code.
 */
class SemanticAnalyserTests extends Compiler[Program] with TestCompiler[Program] {

    import PrologTree.{PrologTree, Program}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}
    import org.bitbucket.inkytonik.kiama.util.{Config, Source}

    filetests("Prolog", "src/org/bitbucket/inkytonik/kiama/example/prolog/tests", ".pl", ".sem")

    val parsers = new SyntaxAnalyser(positions)
    val parser = parsers.program

    /**
     * Process the tree by conducting semantic analysis and reporting any errors.
     */
    def process(source : Source, ast : Program, config : Config) {
        val tree = new PrologTree(ast)
        val analyser = new SemanticAnalyser(tree)
        val messages = analyser.errors
        if (messages.length > 0)
            report(messages, config.error)
    }

    def format(m : Program) : Document =
        emptyDocument

}
