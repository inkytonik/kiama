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
import org.kiama.util.{Console, CompilerWithConfig, Config, Emitter,
    ErrorEmitter, JLineConsole, OutputEmitter}
import scala.collection.immutable.Seq

/**
 * Configuration for the Obr compiler.
 */
abstract class ObrConfig (args : Seq[String]) extends Config (args) {
    lazy val targetPrint = opt[Boolean] ("target", descr = "Print the target tree")
    lazy val riscPrint = opt[Boolean] ("risc", 'a', descr = "Print the RISC tree")
    lazy val envPrint = opt[Boolean] ("env", 's', descr = "Print the global environment")
    lazy val execute = opt[Boolean] ("execute", descr = "Execute the compiled code")
}

/**
 * Obr language implementation compiler driver.
 */
class Driver extends SyntaxAnalyser with CompilerWithConfig[ObrInt,ObrConfig] {

    import ObrTree.ObrTree
    import org.kiama.example.obr.{RISCEncoder, RISCTransformer}
    import org.kiama.example.RISC.{RISC, RISCISA}
    import org.kiama.output.PrettyPrinter.{any => ppany, pretty}
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging.report

    override def createConfig (args : Seq[String],
                               out : Emitter = new OutputEmitter,
                               err : Emitter = new ErrorEmitter) : ObrConfig =
        new ObrConfig (args) {
            lazy val output = out
            lazy val error = err
        }

    def process (filename : String, ast : ObrInt, config : ObrConfig) {

        // Conduct semantic analysis and report any errors
        val tree = new ObrTree (ast)
        val analyser = new SemanticAnalyser (tree)
        val messages = analyser.errors
        if (messages.length > 0) {
            report (messages, config.error)
        } else {
            // Print out final environment
            if (config.envPrint ()) {
                config.output.emitln (analyser.env (ast))
            }

            // Label generator for this run
            val labels = new RISCLabels

            // Compile the source tree to a target tree
            val transformer = new RISCTransformer (analyser, labels)
            val targettree = transformer.code (ast)

            // Print out the target tree for debugging
            if (config.targetPrint ()) {
                config.output.emitln (pretty (ppany (targettree)))
            }

            // Encode the target tree and emit the assembler or run if requested
            val encoder = new RISCEncoder (labels)
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

    import ObrTree.ObrTree
    import org.kiama.util.Messaging.report

    override def process (filename : String, ast : ObrInt, config : ObrConfig) {

        // Conduct semantic analysis and report any errors
        val tree = new ObrTree (ast)
        val analyser = new SemanticAnalyser (tree)
        val messages = analyser.errors
        if (messages.length > 0)
            report (messages, config.error)

    }

}
