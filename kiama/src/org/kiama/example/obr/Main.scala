/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2013 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2013 Dominic Verity, Macquarie University.
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
import org.kiama.attribution.Attribution.initTree
import org.kiama.util.Compiler

/**
 * Obr language implementation compiler driver.
 */
class Driver extends SyntaxAnalysis with Compiler[ObrInt] {

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
    val usage = """|usage: scala org.obr.compiler.Main [options] file.obr
                   |options:    -t spill target tree to stdout
                   |            -a spill RISC assembler to stdout
                   |            -s spill global environment to stdout
                   |            -e run compiled code""".stripMargin

    /**
     * The following flags correspond to the command line arguments
     */
    var spillTargetTreeFlag = false
    var spillRISCAssemFlag = false
    var spillEnvirFlag = false
    var execFlag : Boolean = false

    /**
     * Scan command line arguments to handle any compiler switches.
     */
    override def checkargs (args : Array[String], emitter : Emitter) : Array[String] = {
        def checkFlag (arg : String) : Boolean =
            arg match {
                case "-t" => { spillTargetTreeFlag = true; false }
                case "-a" => { spillRISCAssemFlag = true; false }
                case "-s" => { spillEnvirFlag = true; false }
                case "-e" => { execFlag = true; false }
                case _ => true
            }
        for (a <- args; if (checkFlag (a))) yield (a)
    }

    /**
     * Function to process the input that was parsed.  console and emitter
     * are used for input and output.  Return true if everything worked, false
     * otherwise.
     */
    override def process (filename : String, ast : ObrInt, console : Console, emitter : Emitter) : Boolean = {

        super.process (filename, ast, console, emitter)

        // Initialise compiler state
        SymbolTable.reset ()
        RISCTree.reset ()

        // Conduct semantic analysis and report any errors
        ast->errors
        if (messagecount > 0) {
            report (emitter)
            false
        } else {
            // Print out final environment
            if (spillEnvirFlag) {
                emitter.emitln (ast->envout)
            }

            // Compile the source tree to a target tree
            val targettree = ast->code
            initTree (targettree)

            // Print out the target tree for debugging
            if (spillTargetTreeFlag) {
                emitter.emitln (targettree)
            }

            // Encode the target tree and emit the assembler or run if requested
            encode (targettree)

            if (spillRISCAssemFlag) {
                RISCISA.prettyprint (emitter, getassem)
            }

            if (execFlag) {
                val code = getcode
                val machine = new RISC (code, console, emitter)
                machine.run
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
class ParserDriver extends SyntaxAnalysis with Compiler[ObrInt] {

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
    override def process (filename : String, ast : ObrInt, console : Console, emitter : Emitter) : Boolean = {

        // Print ast to the emitter
        emitter.emitln (ast.toString)
        true

    }

}

/**
 * A driver which parses a program file and runs the semantic analyser.
 */
class SemanticDriver extends SyntaxAnalysis with Compiler[ObrInt] {

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
    override def process (filename : String, ast : ObrInt, console : Console, emitter : Emitter) : Boolean = {

        // Initialise compiler state
        SymbolTable.reset ()

        // Conduct semantic analysis and report any errors
        initTree (ast)
        ast->errors
        if (messagecount > 0) {
            report (emitter)
            false
        } else
            true

    }

}
