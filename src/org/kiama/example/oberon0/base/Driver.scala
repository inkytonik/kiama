/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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

import base.source.ModuleDecl
import org.kiama.util.Compiler
import org.kiama.attribution.Attribution.initTree
import org.kiama.attribution.Attribution.resetMemo
import org.kiama.util.Console
import org.kiama.util.Emitter
import org.kiama.output.PrettyPrinter
import scala.util.parsing.combinator.RegexParsers

/**
 * A driver for an artefact that parses, pretty prints and performs semantic
 * analysis.
 */
trait Driver extends Compiler[ModuleDecl] with PrettyPrinter {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser =>

    import org.kiama.util.IO.{filereader, FileNotFoundException}
    import org.kiama.util.Messaging.{message, messagecount, report,
        resetmessages, sortedmessages}

    /**
     * The name of this artefact.
     */
    def artefact : String

    // Command-line argument handling

    def usageMessage : String =
        """|Usage: driver <options> <filename>
           |Options:
           |   -h   print this help message and stop
           |   -a   print the abstract syntax tree
           |   -A   pretty-print the abstract syntax tree
           |   -x   run in LDTA challenge mode""".stripMargin

    var helpFlag : Boolean = _
    var printastFlag : Boolean = _
    var pprintastFlag : Boolean = _
    var challengeFlag : Boolean = _
    var input : Option[String] = None

    val helpFlagDefault = false
    val printastFlagDefault = false
    val pprintastFlagDefault = false
    val challengeFlagDefault = false

    def resetflags () {
        helpFlag = helpFlagDefault
        printastFlag = printastFlagDefault
        pprintastFlag = pprintastFlagDefault
        challengeFlag = challengeFlagDefault
        input = None
    }

    def processargs (args : List[String]) : Boolean =
        args match {
            case Nil =>
                input != None
            case arg :: rest =>
                if (input != None)
                    false
                else if (arg.startsWith ("-")) {
                    if (arg == "-h") {
                        helpFlag = true
                        processargs (rest)
                    } else if (arg == "-a") {
                        printastFlag = true
                        processargs (rest)
                    } else if (arg == "-A") {
                        pprintastFlag = true
                        processargs (rest)
                    } else if (arg == "-x") {
                        challengeFlag = true
                        processargs (rest)
                    } else
                        false
                } else {
                    input = Some (arg)
                    processargs (rest)
                }
        }

    /**
     * Process the command-line arguments and return an array of the input
     * file names if processing should continue, None otherwise.
     */
    override def checkargs (args : Array[String], emitter : Emitter) : Array[String] = {
        resetflags ()
        if (processargs (args.toList)) {
            if (helpFlag) {
                emitter.emitln (usageMessage)
                Array.empty
            } else
                Array (input.get)
        } else {
            emitter.emitln ("Program arguments were " + args.mkString (" "))
            emitter.emitln (usageMessage)
            Array.empty
        }
    }

    /**
     * Output a section heading so that the output can be split later.
     */
    def section (emitter : Emitter, name : String) {
        emitter.emitln ("* " + name)
    }

    /**
     * Custom driver for section tagging and challenge mode for errors.  If
     * a parse error occurs: in challenge mode, just send "parse failed" to
     * standard output, otherwise send the message to the errors file.
     */
    override def driver (args : Array[String], console : Console, emitter : Emitter) {
        val newargs = checkargs (args, emitter)
        try {
            for (arg <- newargs) {
                val reader = filereader (newargs (0))
                makeast (reader, newargs (0), emitter) match {
                    case Left (ast) =>
                        process (ast, console, emitter)
                    case Right (msg) =>
                        if (challengeFlag) {
                            section (emitter, "stdout")
                            emitter.emitln ("parse failed")
                        }
                        section (emitter, "errors")
                        emitter.emitln (msg)
                }
            }
        } catch {
            case e : FileNotFoundException =>
                emitter.emitln (e.getMessage)
        }
    }

    /**
     * Perform initialisation of semantic analysis that is necessary before
     * processing an AST.
     */
    def initialiseSemanticAnalysis {
        resetEnvironments
        resetmessages
        resetMemo
    }

    /**
     * Process the given abstract syntax tree.  Send output to emitter,
     * marking sections so that we can split things later.
     */
    override def process (ast : ModuleDecl, console : Console, emitter : Emitter) : Boolean = {

        // Perform default processing
        super.process (ast, console, emitter)

        if (printastFlag) {
            section (emitter, "ast")
            emitter.emitln (pretty_any (ast))
        }
        if (pprintastFlag) {
            section (emitter, "_pp.ob")
            emitter.emitln (pretty (toDoc (ast)))
        }

        // Perform semantic analysis
        initialiseSemanticAnalysis
        check (ast)
        if (messagecount == 0) {

            // No semantic errors, go on to process the AST as appropriate
            val nast = processast (ast, console, emitter)

            // Consume the processed AST (e.g., by generating code from it)
            consumeast (nast, console, emitter)

            true

        } else {

            // Semantic analysis failed, abort.  If in challenge mode, report
            // line number of first error to standard output.  Make full report
            // to errors file.
            if (challengeFlag) {
                section (emitter, "stdout")
                emitter.emitln ("line " + sortedmessages (0).pos.line)
            }
            section (emitter, "errors")
            report (emitter)
            false

        }

    }

    /**
     * Process the AST, returning the new one.  By default, return the AST unchanged.
     */
    def processast (ast : ModuleDecl, console : Console, emitter : Emitter) : ModuleDecl =
        ast

    /**
     * Consume the AST. For example, translate it to something else. By default, do
     * nothing.
     */
    def consumeast (ast : ModuleDecl, console : Console, emitter : Emitter) {
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic
 * analysis and transforms.
 */
trait TransformingDriver extends Driver {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser with Transformer =>

    override def usageMessage : String =
        super.usageMessage + """
        |   -i   print the intermediate abstract syntax tree
        |   -I   pretty-print the intermediate abstract syntax tree""".stripMargin

    var printiastFlag : Boolean = _
    var pprintiastFlag : Boolean = _

    val printiastFlagDefault = false
    val pprintiastFlagDefault = false

    override def resetflags () {
        super.resetflags
        printiastFlag = printiastFlagDefault
        pprintiastFlag = pprintiastFlagDefault
    }

    override def processargs (args : List[String]) : Boolean =
        args match {
            case Nil =>
                input != None
            case arg :: rest =>
                if (input != None)
                    false
                else if (arg.startsWith ("-")) {
                    if (arg == "-i") {
                        printiastFlag = true
                        processargs (rest)
                    } else if (arg == "-I") {
                        pprintiastFlag = true
                        processargs (rest)
                    } else
                        super.processargs (args)
                } else {
                    input = Some (arg)
                    processargs (rest)
                }
        }

    /**
     * Process the AST by transforming it.  Then apply higher-level transformations.
     */
    override def processast (ast : ModuleDecl, console : Console, emitter : Emitter) : ModuleDecl = {
        initialiseSemanticAnalysis
        val nast = transform (ast)
        if (printiastFlag) {
            section (emitter, "iast")
            emitter.emitln (pretty_any (nast))
        }
        if (challengeFlag)
            section (emitter, "_lifted.ob")
        else if (pprintiastFlag)
            section (emitter, "_ipp.ob")
        if (pprintiastFlag || challengeFlag)
            emitter.emitln (pretty (toDoc (nast)))
        initTree (nast)
        nast
    }

}

/**
 * A driver for an artefact that parses, pretty prints, performs semantic
 * analysis, transforms and translates.
 */
trait TranslatingDriver extends TransformingDriver {

    this : RegexParsers with source.SourcePrettyPrinter with SymbolTable
        with Analyser with Transformer with Translator with c.CPrettyPrinter =>

    override def usageMessage : String =
        super.usageMessage + """
        |   -c   print the C abstract syntax tree
        |   -C   pretty-print the C abstract syntax tree""".stripMargin

    var printcastFlag : Boolean = _
    var pprintcastFlag : Boolean = _

    val printcastFlagDefault = false
    val pprintcastFlagDefault = false

    override def resetflags () {
        super.resetflags
        printcastFlag = printcastFlagDefault
        pprintcastFlag = pprintcastFlagDefault
    }

    override def processargs (args : List[String]) : Boolean =
        args match {
            case Nil =>
                input != None
            case arg :: rest =>
                if (input != None)
                    false
                else if (arg.startsWith ("-")) {
                    if (arg == "-c") {
                        printcastFlag = true
                        processargs (rest)
                    } else if (arg == "-C") {
                        pprintcastFlag = true
                        processargs (rest)
                    } else
                        super.processargs (args)
                } else {
                    input = Some (arg)
                    processargs (rest)
                }
        }

    /**
     * Consume the AST by translating it to C.
     */
    override def consumeast (ast : ModuleDecl, console : Console, emitter : Emitter) {
        initialiseSemanticAnalysis
        val nast = translate (ast)
        if (printcastFlag) {
            section (emitter, "cast")
            emitter.emitln (pretty_any (nast))
        }
        if (pprintcastFlag || challengeFlag) {
            section (emitter, "c")
            emitter.emitln (pretty (toDoc (nast)))
        }
    }

}
