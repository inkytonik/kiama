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
import org.kiama.util.{Console, CompilerWithConfig, Config, Emitter,
    JLineConsole}
import scala.collection.immutable.Seq

/**
 * Configuration for the Obr compiler.
 */
class ObrConfig (args : Seq[String], emitter : Emitter) extends Config (args, emitter) {
    val targetPrint = opt[Boolean] ("target", descr = "Print the target tree")
    val riscPrint = opt[Boolean] ("risc", 'a', descr = "Print the RISC tree")
    val envPrint = opt[Boolean] ("env", 's', descr = "Print the global environment")
    val execute = opt[Boolean] ("execute", descr = "Execute the compiled code")
}

/**
 * Obr language implementation compiler driver.
 */
class Driver extends SyntaxAnalysis with CompilerWithConfig[ObrInt,ObrConfig] {

    import org.kiama.example.obr.{RISCEncoder, RISCTransformation}
    import org.kiama.example.RISC.{RISC, RISCISA}
    import org.kiama.util.{Emitter, Messaging}

    override def createConfig (args : Seq[String], emitter : Emitter = new Emitter) : ObrConfig =
        new ObrConfig (args, emitter)

    override def process (filename : String, ast : ObrInt, config : ObrConfig) {

        super.process (filename, ast, config)

        // Initialise compiler state
        SymbolTable.reset ()
        RISCLabels.reset ()

        // Conduct semantic analysis and report any errors
        val messaging = new Messaging
        val analysis = new SemanticAnalysis (messaging)
        analysis.errors (ast)
        if (messaging.messagecount > 0) {
            messaging.report (config.emitter)
        } else {
            // Print out final environment
            if (config.envPrint ()) {
                config.emitter.emitln (analysis.envout (ast))
            }

            // Compile the source tree to a target tree
            val transformer = new RISCTransformation (analysis)
            val targettree = transformer.code (ast)
            initTree (targettree)

            // Print out the target tree for debugging
            if (config.targetPrint ()) {
                config.emitter.emitln (targettree)
            }

            // Encode the target tree and emit the assembler or run if requested
            val encoder = new RISCEncoder
            encoder.encode (targettree)

            if (config.riscPrint ()) {
                RISCISA.prettyprint (config.emitter, encoder.getassem)
            }

            if (config.execute ()) {
                val code = encoder.getcode
                val machine = new RISC (code, config.console (), config.emitter)
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
        config.emitter.emitln (ast.toString)
    }

}

/**
 * A driver which parses a program file and runs the semantic analyser.
 */
class SemanticDriver extends Driver {

    import org.kiama.util.Messaging

    override def process (filename : String, ast : ObrInt, config : ObrConfig) {

        // Initialise compiler state
        SymbolTable.reset ()

        // Conduct semantic analysis and report any errors
        val messaging = new Messaging
        val analysis = new SemanticAnalysis (messaging)
        initTree (ast)
        analysis.errors (ast)
        if (messaging.messagecount > 0)
            messaging.report (config.emitter)

    }

}
