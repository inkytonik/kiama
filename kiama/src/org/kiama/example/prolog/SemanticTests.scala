/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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
class SemanticTests extends SemanticTestDriver {

    filetests ("Prolog", "kiama/src/org/kiama/example/prolog/test", ".pl", ".sem")

}

class SemanticTestDriver extends SyntaxAnalysis with Compiler[Program]
        with TestCompiler[Program] {

    import SemanticAnalysis._
    import java.io.FileReader
    import org.kiama.util.Messaging._
    import org.kiama.util.Console
    import org.kiama.util.Emitter

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: run file.pl"

    /**
     * Process the tree by conducting semantic analysis and reporting any errors.
     */
    override def process (filename : String, ast : Program, console : Console, emitter : Emitter) : Boolean = {
        super.process (filename, ast, console, emitter)
        resetmessages
        check (ast)
        if (messagecount > 0) {
            report (emitter)
            false
        } else
            true
    }

}
