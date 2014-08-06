/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package base

import source.ModuleDecl
import source.SourceTree.SourceTree
import org.kiama.util.{CompilerWithConfig, Config, Emitter, ErrorEmitter,
    OutputEmitter}
import org.kiama.output.PrettyPrinter
import scala.collection.immutable.Seq
import scala.util.parsing.combinator.RegexParsers

/**
 * Common functionality for all forms of Oberon0 driver.
 */
trait Driver {

    /**
     * The name of this artefact.
     */
    def artefact : String

    /**
     * Output a section heading so that the output can be split later.
     */
    def section (emitter : Emitter, name : String) {
        emitter.emitln (s"* $name")
    }

}

/**
 * Configuration for an Oberon0 compiler. For simplicity the different kinds
 * of compiler share a configuration type, so some of these settings have no
 * effect for some of the drivers.
 */
class Oberon0Config (args : Seq[String], output : Emitter, errors : Emitter, testPrettyPrint : Boolean = false) extends Config (args, output, errors) {
    val challenge = opt[Boolean] ("challenge", 'x', descr = "Run in LDTA challenge mode")
    val astPrint = opt[Boolean] ("astPrint", 'a', descr = "Print the abstract syntax tree")
    val astPrettyPrint = opt[Boolean] ("astPrettyPrint", 'A', descr = "Pretty-print the abstract syntax tree",
                                       default = Some (testPrettyPrint))
    val intPrint = opt[Boolean] ("intPrint", 'i', descr = "Print the intermediate abstract syntax tree")
    val intPrettyPrint = opt[Boolean] ("intPrettyPrint", 'I', descr = "Pretty-print the intermediate abstract syntax tree")
    val cPrint = opt[Boolean] ("cPrint", 'c', descr = "Print the C abstract syntax tree")
    val cPrettyPrint = opt[Boolean] ("cPrettyPrint", 'C', descr = "Pretty-print the C abstract syntax tree",
                                     default = Some (testPrettyPrint))
}

/**
 * A driver for an artefact that parses, pretty prints and performs semantic
 * analysis.
 */
trait FrontEndDriver extends Driver with CompilerWithConfig[ModuleDecl,Oberon0Config] {

    this : RegexParsers with source.SourcePrettyPrinter =>

    import java.io.File
    import org.kiama.attribution.Attribution.resetMemo
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging.{report, sortmessages}
    import org.kiama.util.IO.{filereader, FileNotFoundException}

    override def createConfig (args : Seq[String],
                               output : Emitter = new OutputEmitter,
                               error : Emitter = new ErrorEmitter) : Oberon0Config =
        new Oberon0Config (args, output, error)

    /**
     * Custom driver for section tagging and challenge mode for errors.  If
     * a parse error occurs: in challenge mode, just send "parse failed" to
     * standard output, otherwise send the message to the errors file.
     */
    override def processfile (filename : String, config : Oberon0Config) {
        try {
            val output = config.output
            val reader = filereader (filename)
            makeast (reader, filename, config) match {
                case Left (ast) =>
                    process (filename, ast, config)
                case Right (msg) =>
                    if (config.challenge ()) {
                        section (output, "stdout")
                        output.emitln ("parse failed")
                    }
                    section (output, "errors")
                    output.emitln (msg)
            }
        } catch {
            case e : FileNotFoundException =>
                config.error.emitln (e.getMessage)
        }
    }

    /**
     * A builder of the analysis phase for this driver.
     */
    def buildAnalyser (tree : SourceTree) : Analyser

    /**
     * Process the given abstract syntax tree.  Send output to emitter,
     * marking sections so that we can split things later.
     */
    override def process (filename : String, ast : ModuleDecl, config : Oberon0Config) {

        val output = config.output

        // Perform default processing
        super.process (filename, ast, config)

        if (config.astPrint ()) {
            section (output, "ast")
            output.emitln (pretty_any (ast))
        }
        if (config.astPrettyPrint ()) {
            section (output, "_pp.ob")
            output.emitln (pretty (toDoc (ast)))
        }

        // Make a tree for this program
        val tree = new SourceTree (ast)

        // Build the phases for this driver
        val analyser = buildAnalyser (tree)

        // Perform semantic analysis
        analyser.resetEnvironments
        resetMemo
        val messages = analyser.errors (ast)

        if (messages.length == 0) {

            // No semantic errors, go on to process the AST as appropriate
            val ntree = processast (tree, config)

            // Consume the processed AST (e.g., by generating code from it)
            consumeast (ntree, config)

        } else {

            // Semantic analysis failed, abort.  If in challenge mode, report
            // line number of first error to standard output.  Make full report
            // to errors file.
            if (config.challenge ()) {
                section (output, "stdout")
                val line = sortmessages (messages).head.line
                output.emitln (s"line $line")
            }
            section (output, "errors")
            report (messages, output)

        }

    }

    /**
     * Process a tree, returning the new one.  By default, return the tree unchanged.
     */
    def processast (tree : SourceTree, config : Oberon0Config) : SourceTree =
        tree

    /**
     * Consume the AST. For example, translate it to something else. By default, do
     * nothing.
     */
    def consumeast (tree : SourceTree, config : Oberon0Config) {
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic
 * analysis and transforms.
 */
trait TransformingDriver extends FrontEndDriver with CompilerWithConfig[ModuleDecl,Oberon0Config] {

    this : RegexParsers with source.SourcePrettyPrinter =>

    /**
     * A builder of the transformer phase for this driver.
     */
    def buildTransformer (tree : SourceTree) : Transformer

    /**
     * Process the AST by transforming it.
     */
    override def processast (tree : SourceTree, config : Oberon0Config) : SourceTree = {
        val output = config.output
        val transformer = buildTransformer (tree)
        val ntree = transformer.transform (tree)
        if (config.intPrint ()) {
            section (output, "iast")
            output.emitln (pretty_any (ntree.root))
        }
        if (config.challenge ())
            section (output, "_lifted.ob")
        else if (config.intPrettyPrint ())
            section (output, "_ipp.ob")
        if (config.intPrettyPrint () || config.challenge ())
            output.emitln (pretty (toDoc (ntree.root)))
        ntree
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic
 * analysis, transforms and translates.
 */
trait TranslatingDriver extends TransformingDriver with CompilerWithConfig[ModuleDecl,Oberon0Config] {

    this : RegexParsers with source.SourcePrettyPrinter with c.CPrettyPrinter =>

    /**
     * A builder of the transformer phase for this driver.
     */
    def buildTranslator (tree : SourceTree) : Translator

    /**
     * Consume the AST by translating it to C.
     */
    override def consumeast (tree : SourceTree, config : Oberon0Config) {
        val output = config.output
        val translator = buildTranslator (tree)
        val cast = translator.translate (tree.root) // FIXME should be tree
        if (config.cPrint ()) {
            section (output, "cast")
            output.emitln (pretty_any (cast))
        }
        if (config.cPrettyPrint () || config.challenge ()) {
            section (output, "c")
            output.emitln (pretty (toDoc (cast)))
        }
    }

}
