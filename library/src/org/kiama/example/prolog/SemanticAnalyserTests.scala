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
package example.prolog

import PrologTree.Program
import org.kiama.util.{Compiler, TestCompiler}

/**
 * Tests that check that the semantic analyser works correctly.  I.e., it correctly
 * diagnoses errors where they are present, and passes correct code.
 */
class SemanticAnalyserTests extends SyntaxAnalyser with Compiler[Program]
        with TestCompiler[Program] {

    import PrologTree.PrologTree
    import org.kiama.util.Config
    import org.kiama.util.Messaging.report

    filetests ("Prolog", "library/src/org/kiama/example/prolog/tests", ".pl", ".sem")

    /**
     * For the purposes of tests, the parser we want is the program one.
     */
    val parser = program

    /**
     * Process the tree by conducting semantic analysis and reporting any errors.
     */
    def process (filename : String, ast : Program, config : Config) {
        val tree = new PrologTree (ast)
        val analyser = new SemanticAnalyser (tree)
        val messages = analyser.errors
        if (messages.length > 0)
            report (messages, config.error)
    }

}
