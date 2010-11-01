/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
 *
 * Contributed by Ben Mockler.
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

import org.kiama.util.RegexCompiler
import compiler.AST._
import compiler.Parser

/**
 * Main program for the Oberon0 implementation.  Parses and checks
 * the Oberon0 program named on the command line.  If checking passes,
 * then encodes the program as RISC assembler and runs the assembler
 * using a machine simulator.
 */
class Driver extends Parser with RegexCompiler[ModuleDecl] {

    import assembler.Assembler
    import compiler.ErrorCheck.collectErrors
    import compiler.Encoder.EncodeModule
    import java.io.FileReader
    import machine.RISC
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.kiama.example.oberon0.Main file.ob0"

    /**
     * Function to process the input that was parsed.  emitter is
     * used for output.  Return true if everything worked, false
     * otherwise.
     */
    def process (ast : ModuleDecl, console : Console, emitter : Emitter) : Boolean = {

        // Check for semantic errors
        ast->collectErrors

        // If everything was OK
        if (messagecount == 0) {
            // Clear the machine code buffer
            Assembler.resetcode
            // Encode the module to RISC assembler
            EncodeModule (ast)
            // Assemble to machine code
            val instrs = Assembler.getcode
            // Run the machine code
            val mymachine = new RISC (instrs, console, emitter)
            mymachine.run
            true
        } else {
            report (emitter)
            false
        }

    }

}

/**
 * Oberon0 language implementation main program.
 */
object Main extends Driver
