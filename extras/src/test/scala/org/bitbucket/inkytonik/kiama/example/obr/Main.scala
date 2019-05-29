/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2019 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.obr

import ObrTree.{ObrInt, ObrNode}
import org.bitbucket.inkytonik.kiama.util.{
    CompilerWithConfig,
    Config,
    Source
}

/**
 * Configuration for the Obr compiler.
 */
class ObrConfig(args : Seq[String]) extends Config(args) {
    lazy val targetPrint = opt[Boolean]("target", descr = "Print the target tree")
    lazy val riscPrint = opt[Boolean]("risc", 'a', descr = "Print the RISC tree")
    lazy val envPrint = opt[Boolean]("env", 's', descr = "Print the global environment")
    lazy val execute = opt[Boolean]("execute", descr = "Execute the compiled code")
}

/**
 * Obr language implementation compiler driver.
 */
class Driver extends CompilerWithConfig[ObrNode, ObrInt, ObrConfig] {

    import ObrTree.ObrTree
    import org.bitbucket.inkytonik.kiama.example.obr.{RISCEncoder, RISCTransformer}
    import org.bitbucket.inkytonik.kiama.example.RISC.{RISC, RISCISA}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, pretty}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult

    override def createConfig(args : Seq[String]) : ObrConfig =
        new ObrConfig(args)

    val name = "obr"

    def parse(source : Source) : ParseResult[ObrInt] = {
        val parsers = new SyntaxAnalyser(positions)
        parsers.parseAll(parsers.program, source)
    }

    def process(source : Source, ast : ObrInt, config : ObrConfig) : Unit = {

        // Conduct semantic analysis and report any errors
        val tree = new ObrTree(ast)
        val analyser = new SemanticAnalyser(tree)
        val messages = analyser.errors
        if (messages.length > 0) {
            report(source, messages, config)
        } else {
            // Print out final environment
            if (config.envPrint()) {
                config.output().emitln(analyser.env(ast))
            }

            // Label generator for this run
            val labels = new RISCLabels

            // Compile the source tree to a target tree
            val transformer = new RISCTransformer(analyser, labels)
            val targettree = transformer.code(ast)

            // Print out the target tree for debugging
            if (config.targetPrint()) {
                config.output().emitln(pretty(any(targettree)))
            }

            // Encode the target tree and emit the assembler or run if requested
            val encoder = new RISCEncoder(labels)
            encoder.encode(targettree)

            if (config.riscPrint()) {
                RISCISA.prettyprint(config.output(), encoder.getassem)
            }

            if (config.execute()) {
                val code = encoder.getcode
                val machine = new RISC(code, config.console(), config.output())
                machine.run
            }
        }

    }

    def format(ast : ObrInt) : Document =
        emptyDocument

}

/**
 * Obr language implementation main program.
 */
object Main extends Driver

/**
 * The next driver simply spills the abstract syntax tree to the console.
 */
class ParserDriver extends Driver {

    override def process(source : Source, ast : ObrInt, config : ObrConfig) : Unit = {
        config.output().emitln(ast.toString)
    }

}

/**
 * A driver which parses a program file and runs the semantic analyser.
 */
class SemanticDriver extends Driver {

    import ObrTree.ObrTree

    override def process(source : Source, ast : ObrInt, config : ObrConfig) : Unit = {

        // Conduct semantic analysis and report any errors
        val tree = new ObrTree(ast)
        val analyser = new SemanticAnalyser(tree)
        val messages = analyser.errors
        if (messages.length > 0)
            report(source, messages, config)

    }

}
