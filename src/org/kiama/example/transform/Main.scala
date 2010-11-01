/**
 * Main program for transformation compiler.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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
package example.transform

import AST.Program
import org.kiama.util.RegexCompiler

/**
 * Main program for transformation compiler.
 */
class Driver extends Parser with RegexCompiler[Program] {

    import java.io.FileReader
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.kiama.transform.Main file.exp"

    /**
     * Function to process the input that was parsed.  emitter is
     * used for output.  Return true if everything worked, false
     * otherwise.  Here we process the first AST to compute the second
     * AST and print it, then perform semantic analysis on the second
     * AST, print it and print any semantic errors.
     */
    def process (ast : Program, console : Console, emitter : Emitter) : Boolean = {
        resetmessages
        emitter.emitln (ast)
        val exp = Analysis.process (ast)
        emitter.emitln (exp)
        report (emitter)
        true
    }

}

/**
 * Transformation compiler main program.
 */
object Main extends Driver
