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
import org.kiama.attribution.Attribution.initTree
import org.kiama.attribution.Attribution.resetMemo
import org.kiama.util.{CompilerWithConfig, Config, Emitter}
import org.kiama.output.PrettyPrinter
import scala.collection.immutable.Seq
import scala.util.parsing.combinator.RegexParsers

/**
 * Common functionality for all forms of Oberon0 driver.
 */
trait Driver {

    this : SymbolTable =>

    /**
     * The name of this artefact.
     */
    def artefact : String

    /**
     * Perform initialisation of semantic analysis that is necessary before
     * processing an AST.
     */
    def initialiseSemanticAnalysis {
        resetEnvironments
        resetMemo
    }

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
class Oberon0Config (args : Seq[String], emitter : Emitter, testPrettyPrint : Boolean = false) extends Config (args, emitter) {
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

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser =>

    import java.io.File
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging.{report, sortmessages}
    import org.kiama.util.IO.{filereader, FileNotFoundException}

    override def createConfig (args : Seq[String], emitter : Emitter = new Emitter) : Oberon0Config =
        new Oberon0Config (args, emitter)

    /**
     * Custom driver for section tagging and challenge mode for errors.  If
     * a parse error occurs: in challenge mode, just send "parse failed" to
     * standard output, otherwise send the message to the errors file.
     */
    override def processfile (filename : String, config : Oberon0Config) {
        val emitter = config.emitter
        try {
            val reader = filereader (filename)
            makeast (reader, filename, config) match {
                case Left (ast) =>
                    process (filename, ast, config)
                case Right (msg) =>
                    if (config.challenge ()) {
                        section (emitter, "stdout")
                        emitter.emitln ("parse failed")
                    }
                    section (emitter, "errors")
                    emitter.emitln (msg)
            }
        } catch {
            case e : FileNotFoundException =>
                emitter.emitln (e.getMessage)
        }
    }

    /**
     * Process the given abstract syntax tree.  Send output to emitter,
     * marking sections so that we can split things later.
     */
    override def process (filename : String, ast : ModuleDecl, config : Oberon0Config) {

        val emitter = config.emitter

        // Perform default processing
        super.process (filename, ast, config)

        if (config.astPrint ()) {
            section (emitter, "ast")
            emitter.emitln (pretty_any (ast))
        }
        if (config.astPrettyPrint ()) {
            section (emitter, "_pp.ob")
            emitter.emitln (pretty (toDoc (ast)))
        }

        // Perform semantic analysis
        initialiseSemanticAnalysis
        val messages = ast->errors
        if (messages.length == 0) {

            // No semantic errors, go on to process the AST as appropriate
            val nast = processast (ast, config)

            // Consume the processed AST (e.g., by generating code from it)
            consumeast (nast, config)

        } else {

            // Semantic analysis failed, abort.  If in challenge mode, report
            // line number of first error to standard output.  Make full report
            // to errors file.
            if (config.challenge ()) {
                section (emitter, "stdout")
                val line = sortmessages (messages).head.line
                emitter.emitln (s"line $line")
            }
            section (emitter, "errors")
            report (messages, emitter)

        }

    }

    /**
     * Process the AST, returning the new one.  By default, return the AST unchanged.
     */
    def processast (ast : ModuleDecl, config : Oberon0Config) : ModuleDecl =
        ast

    /**
     * Consume the AST. For example, translate it to something else. By default, do
     * nothing.
     */
    def consumeast (ast : ModuleDecl, config : Oberon0Config) {
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic
 * analysis and transforms.
 */
trait TransformingDriver extends FrontEndDriver with CompilerWithConfig[ModuleDecl,Oberon0Config] {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser with Transformer =>

    /**
     * Process the AST by transforming it.  Then apply higher-level transformations.
     */
    override def processast (ast : ModuleDecl, config : Oberon0Config) : ModuleDecl = {
        val emitter = config.emitter
        initialiseSemanticAnalysis
        val nast = transform (ast)
        if (config.intPrint ()) {
            section (emitter, "iast")
            emitter.emitln (pretty_any (nast))
        }
        if (config.challenge ())
            section (emitter, "_lifted.ob")
        else if (config.intPrettyPrint ())
            section (emitter, "_ipp.ob")
        if (config.intPrettyPrint () || config.challenge ())
            emitter.emitln (pretty (toDoc (nast)))
        initTree (nast)
        nast
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic
 * analysis, transforms and translates.
 */
trait TranslatingDriver extends TransformingDriver with CompilerWithConfig[ModuleDecl,Oberon0Config] {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser with Transformer with Translator with c.CPrettyPrinter =>

    /**
     * Consume the AST by translating it to C.
     */
    override def consumeast (ast : ModuleDecl, config : Oberon0Config) {
        val emitter = config.emitter
        initialiseSemanticAnalysis
        val nast = translate (ast)
        if (config.cPrint ()) {
            section (emitter, "cast")
            emitter.emitln (pretty_any (nast))
        }
        if (config.cPrettyPrint () || config.challenge ()) {
            section (emitter, "c")
            emitter.emitln (pretty (toDoc (nast)))
        }
    }

}
