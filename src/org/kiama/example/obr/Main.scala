/**
 * Obr language implementation main program.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
package example.obr

import ObrTree.ObrInt
import org.kiama.util.RegexCompiler

/**
 * Obr language implementation compiler driver.
 */
class Driver extends SyntaxAnalysis with RegexCompiler[ObrInt] {

    import java.io.FileReader
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._
    import SemanticAnalysis._
    import Transformation._

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.kiama.example.obr.Main file.obr"

    /**
     * Function to process the input that was parsed.  emitter is
     * used for output.  Return true if everything worked, false
     * otherwise.
     */
    def process (ast : ObrInt, console : Console, emitter : Emitter) : Boolean = {

        // Initialise compiler state
        SymbolTable.reset
        SPARCTree.reset

        // Conduct semantic analysis and report any errors
        ast->errors
        if (messagecount > 0) {
            report (emitter)
            false
        } else {
            // Compile the source tree to a target tree
            val targettree = ast->code

            // Print out the target tree for debugging
            // println (targettree)

            // Encode the target tree and emit the assembler
            val e = new Encoder (emitter)
            e.encode (targettree)
            true
        }

    }

}

/**
 * Obr language implementation main program.
 */
object Main extends Driver
