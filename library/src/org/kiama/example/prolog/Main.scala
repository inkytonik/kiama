/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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
package example.prolog

import PrologTree.Literal
import org.kiama.output.PrettyPrinter
import org.kiama.util.{Emitter, REPLConfig, ParsingREPLWithConfig}

/**
 * Configuration for the Prolog REPL.
 */
abstract class PrologConfig (args : Seq[String]) extends REPLConfig (args) {

    import org.rogach.scallop.{ArgType, ValueConverter}
    import PrologTree.Program
    import scala.reflect.runtime.universe.TypeTag

    /**
     * Convertor for database option.
     */
    val databaseConverter =
        new ValueConverter[Program] {

            val argType = ArgType.SINGLE

            def parse (s : List[(String, List[String])]) : Either[String,Option[Program]] =
                s match {
                    case List ((_, List (filename))) =>
                        Main.makeDatabase (filename)
                    case _ =>
                        Right (None)
                }

            val tag = implicitly[TypeTag[Program]]

        }

    /**
     * The program that represents the facts and clauses that will be made available
     * to the queries that are entered in the REPL.
     */
    lazy val database = opt[Program] ("database", descr = "Database of facts and clauses to use in queries",
                                      required = true) (databaseConverter)

}

/**
 * Conduct semantic analysis on the Prolog program in the file given as
 * the first command-line argument.  If the program is correct, enter an
 * interactive read-eval-print loop (REPL) to read queries.  For each
 * query, call the interpreter to evaluate it.
 */
object Main extends SyntaxAnalyser with ParsingREPLWithConfig[Literal,PrologConfig] with PrettyPrinter {

    import java.io.FileReader
    import java.io.FileNotFoundException
    import org.kiama.util.Messaging.report
    import org.kiama.util.{Emitter, ErrorEmitter, OutputEmitter, StringEmitter}
    import PrologTree.{Program, PrologTree}

    val banner = "Prolog interpreter (exit with end of file: ^Z on Windows, ^D on Mac, Linux, Unix"

    def createConfig (args : Seq[String],
                      out : Emitter = new OutputEmitter,
                      err : Emitter = new ErrorEmitter) : PrologConfig =
        new PrologConfig (args) {
            lazy val output = out
            lazy val error = err
        }

    /**
     * Helper function to create the database from the given filename or return
     * a command-line error.
     */
    def makeDatabase (filename : String) : Either[String,Option[Program]] =
        try {
            val reader = new FileReader (filename)
            // Parse the file
            parse (program, reader) match {
                // If parse worked, we get a source tree, check it
                case Success (dbtree, _) =>
                    // Pretty print the source tree
                    // cnofig.error.emitln (pretty (product (dbtree)))
                    val tree = new PrologTree (dbtree)
                    val analyser = new SemanticAnalyser (tree)
                    val messages = analyser.errors
                    if (messages.length > 0) {
                        val emitter = new StringEmitter
                        report (messages, emitter)
                        Left (s"database file errors: ${emitter.result}")
                    } else {
                        Right (Some (dbtree))
                    }
                // If parse failed, we get an error message
                case f =>
                    Left (s"error parsing $filename: $f")
            }
        } catch {
            case e : FileNotFoundException =>
                Left (e.getMessage)
        }

    /**
     * The parser to use to parse each line of interactive input. We will
     * read queries, which are just literals followed by a period.
     */
    override val parser = query

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
    def process (querytree : Literal, config : PrologConfig) {
        interpreter.interpret (querytree, config.database (), config.output)
    }

}
