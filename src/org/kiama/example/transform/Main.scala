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

package org.kiama.example.transform

import AST.Program
import org.kiama.util.Compiler

/**
 * Main program for transformation compiler.
 */
class Driver extends Compiler[Program] with Parser {

    import java.io.FileReader
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.kiama.multipass.Main file.exp"

    /**
     * The parser to use to process the input into an AST.
     */
    def parse (filename : String) : Option[Program] = {
        val reader = new FileReader (filename)
        super.parse (parser, reader) match {
            case Success (ast, _) =>
                Some (ast)
            case f =>
                println (f)
                None
        }
    }

    /**
     * Function to process the input that was parsed.  emitter is
     * used for output.  Return true if everything worked, false
     * otherwise.  Here we process the first AST to compute the second
     * AST and print it, then perform semantic analysis on the second
     * AST and print any errors.
     */
    def process (prog : Program, emitter : Emitter) : Boolean = {
        resetmessages
        emitter.emitln (prog)
        val exp = Analysis.process (prog)
        emitter.emitln (exp)
        for (m <- sortedmessages)
            emitter.emitln (m)
        true
    }

}

/**
 * Transformation compiler main program.
 */
object Main extends Driver
