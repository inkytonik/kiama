/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.prolog

import PrologTree.Literal
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter
import org.bitbucket.inkytonik.kiama.util.{REPLConfig, ParsingREPLWithConfig}

/**
 * Configuration for the Prolog REPL.
 */
class PrologConfig(args : Seq[String]) extends REPLConfig(args) {

    import org.rogach.scallop.{ArgType, ValueConverter}
    import PrologTree.Program
    import scala.reflect.runtime.universe.TypeTag

    /**
     * Convertor for database option.
     */
    val databaseConverter =
        new ValueConverter[Program] {

            val argType = ArgType.SINGLE

            def parse(s : List[(String, List[String])]) : Either[String, Option[Program]] =
                s match {
                    case List((_, List(filename))) =>
                        Main.makeDatabase(filename)
                    case _ =>
                        Right(None)
                }

            val tag = implicitly[TypeTag[Program]]

        }

    /**
     * The program that represents the facts and clauses that will be made available
     * to the queries that are entered in the REPL.
     */
    lazy val database = opt[Program]("database", descr = "Database of facts and clauses to use in queries",
        required = true)(databaseConverter)

}

/**
 * Conduct semantic analysis on the Prolog program in the file given as
 * the first command-line argument.  If the program is correct, enter an
 * interactive read-eval-print loop (REPL) to read queries.  For each
 * query, call the interpreter to evaluate it.
 */
class PrologDriver extends ParsingREPLWithConfig[Literal, PrologConfig] with PrettyPrinter {

    import org.bitbucket.inkytonik.kiama.parsing.Success
    import org.bitbucket.inkytonik.kiama.util.{
        FileSource,
        Source,
        StringEmitter
    }
    import PrologTree.{Program, PrologTree}

    val banner = "Prolog interpreter (exit with end of file: ^Z on Windows, ^D on Mac, Linux, Unix"

    def createConfig(args : Seq[String]) : PrologConfig =
        new PrologConfig(args)

    val parsers = new SyntaxAnalyser(positions)
    val parser = parsers.query

    /**
     * Helper function to create the database from the given filename or return
     * a command-line error.
     */
    def makeDatabase(filename : String) : Either[String, Option[Program]] =
        try {
            // Parse the file
            val source = FileSource(filename)
            parsers.parseAll(parsers.program, source) match {
                // If parse worked, we get a source tree, check it
                case Success(dbtree, _) =>
                    // Pretty print the source tree
                    // config.error.emitln (pretty (any (dbtree)))
                    val tree = new PrologTree(dbtree)
                    val analyser = new SemanticAnalyser(tree)
                    val messages = analyser.errors
                    if (messages.length > 0) {
                        val emitter = new StringEmitter
                        report(source, messages, emitter)
                        Left(s"database file errors: ${emitter.result}")
                    } else {
                        Right(Some(dbtree))
                    }
                // If parse failed, we get an error message
                case f =>
                    Left(s"error parsing $filename: $f")
            }
        } catch {
            case e : java.io.FileNotFoundException =>
                Left(e.getMessage)
        }

    /**
     * The prompt to print before each line of input is read.
     */
    override val prompt = "?- "

    /**
     * The interpreter to use to evaluate queries.
     */
    val interpreter = new Interpreter

    /**
     * Process a query by passing it and the program to the interpreter.
     */
    def process(source : Source, querytree : Literal, config : PrologConfig) {
        interpreter.interpret(querytree, config.database(), config.output())
    }

}

object Main extends PrologDriver
