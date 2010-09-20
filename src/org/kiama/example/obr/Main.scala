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

package org.kiama.example.obr

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
    import RISCEncoder.{code => _, _}
    import RISCTransformation._
    import org.kiama.example.RISC.RISCISA
    import org.kiama.example.RISC.RISC

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.kiama.example.org.obr.Main file.obr"

    /**
     * Override this flag and set it to true if you want to execute the
     * generated code, in preference to the default behaviour which is to
     * spill that code to the standard output.
     */
    val execFlag : Boolean = false

    /**
     * Function to process the input that was parsed.  console and emitter
     * are used for input and output.  Return true if everything worked, false
     * otherwise.
     */
    def process (ast : ObrInt, console : Console, emitter : Emitter) : Boolean = {

        // Initialise compiler state
        SymbolTable.reset ()
        RISCTree.reset ()

        // Conduct semantic analysis and report any errors
        ast->errors
        if (messagecount > 0) {
            report
            false
        } else {
            // Compile the source tree to a target tree
            val targettree = ast->code

            // Print out the target tree for debugging
            // println (targettree)

            // Encode the target tree and emit the assembler or run if requested
            encode (targettree)
            if (execFlag) {
                val code = getcode
                val machine = new RISC (code, console, emitter)
                machine.run
            } else {
                RISCISA.prettyprint (emitter, getassem)
            }
            true
        }

    }

}

/**
 * Obr language implementation main program.
 */
object Main extends Driver

/**
 * The next driver simply spills the abstract syntax tree to the console.
 */
class ParserDriver extends SyntaxAnalysis with RegexCompiler[ObrInt] {

    import java.io.FileReader
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.kiama.example.org.obr.Main file.obr"

    /**
     * Function to process the input that was parsed.  console and emitter
     * are used for input and output.  Return true if everything worked, false
     * otherwise.
     */
    def process (ast : ObrInt, console : Console, emitter : Emitter) : Boolean = {

        // Print ast to the emitter
        emitter.emitln (ast.toString)
        true

    }

}

/**
 * Finally a driver which parses a program file and runs the semantic analyser.
 */
class SemanticDriver extends SyntaxAnalysis with RegexCompiler[ObrInt] {

    import java.io.FileReader
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._
    import SemanticAnalysis._
    import RISCTransformation._

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: scala org.kiama.example.org.obr.Main file.obr"

    /**
     * Function to process the input that was parsed.  console and emitter
     * are used for input and output.  Return true if everything worked, false
     * otherwise.
     */
    def process (ast : ObrInt, console : Console, emitter : Emitter) : Boolean = {

        // Initialise compiler state
        SymbolTable.reset ()

        // Conduct semantic analysis and report any errors

        ast->errors
        if (messagecount > 0) {
            report (emitter)
            false
        } else
            true

    }

}

