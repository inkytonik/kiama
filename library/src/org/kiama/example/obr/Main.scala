/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2014 Dominic Verity, Macquarie University.
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
import org.kiama.util.{Console, CompilerWithConfig, Config, Emitter,
    ErrorEmitter, JLineConsole, OutputEmitter}
import scala.collection.immutable.Seq

/**
 * Configuration for the Obr compiler.
 */
class ObrConfig (args : Seq[String], output : Emitter, error : Emitter) extends Config (args, output, error) {
    val targetPrint = opt[Boolean] ("target", descr = "Print the target tree")
    val riscPrint = opt[Boolean] ("risc", 'a', descr = "Print the RISC tree")
    val envPrint = opt[Boolean] ("env", 's', descr = "Print the global environment")
    val execute = opt[Boolean] ("execute", descr = "Execute the compiled code")
}

/**
 * Obr language implementation compiler driver.
 */
class Driver extends SyntaxAnalyser with CompilerWithConfig[ObrInt,ObrConfig] {

    import org.kiama.example.obr.{RISCEncoder, RISCTransformer}
    import org.kiama.example.RISC.{RISC, RISCISA}
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging.report

    override def createConfig (args : Seq[String],
                               output : Emitter = new OutputEmitter,
                               error : Emitter = new ErrorEmitter) : ObrConfig =
        new ObrConfig (args, output, error)

    override def process (filename : String, ast : ObrInt, config : ObrConfig) {

        super.process (filename, ast, config)

        // Initialise compiler state
        SymbolTable.reset ()
        RISCLabels.reset ()

        // Conduct semantic analysis and report any errors
        val analyser = new SemanticAnalyser
        val messages = analyser.errors (ast)
        if (messages.length > 0) {
            report (messages, config.error)
        } else {
            // Print out final environment
            if (config.envPrint ()) {
                config.output.emitln (analyser.env (ast))
            }

            // Compile the source tree to a target tree
            val transformer = new RISCTransformer (analyser)
            val targettree = transformer.code (ast)
            initTree (targettree)

            // Print out the target tree for debugging
            if (config.targetPrint ()) {
                config.output.emitln (targettree)
            }

            // Encode the target tree and emit the assembler or run if requested
            val encoder = new RISCEncoder
            encoder.encode (targettree)

            if (config.riscPrint ()) {
                RISCISA.prettyprint (config.output, encoder.getassem)
            }

            if (config.execute ()) {
                val code = encoder.getcode
                val machine = new RISC (code, config.console (), config.output)
                machine.run
            }
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
class ParserDriver extends Driver {

    override def process (filename : String, ast : ObrInt, config : ObrConfig) {
        config.output.emitln (ast.toString)
    }

}

/**
 * A driver which parses a program file and runs the semantic analyser.
 */
class SemanticDriver extends Driver {

    import org.kiama.util.Messaging.report

    override def process (filename : String, ast : ObrInt, config : ObrConfig) {

        // Initialise compiler state
        SymbolTable.reset ()

        // Conduct semantic analysis and report any errors
        val analyser = new SemanticAnalyser
        initTree (ast)
        val messages = analyser.errors (ast)
        if (messages.length > 0)
            report (messages, config.error)

    }

}
