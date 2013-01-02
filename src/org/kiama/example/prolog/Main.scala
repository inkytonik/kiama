/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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

import org.kiama.output.PrettyPrinter
import org.kiama.util.ParsingREPL

import PrologTree.Literal

/**
 * Conduct semantic analysis on the Prolog program in the file given as
 * the first command-line argument.  If the program is correct, enter an
 * interactive read-eval-print loop (REPL) to read queries.  For each
 * query, call the interpreter to evaluate it.
 */
object Main extends SyntaxAnalysis with ParsingREPL[Literal] with PrettyPrinter {

    import PrologTree.Program
    import SemanticAnalysis._
    import Interpreter._
    import java.io.FileReader
    import java.io.FileNotFoundException
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    override def main (args : Array[String]) {

        args.size match {
            // If there is exactly one command-line argument
            case 1 =>
                try {
                    // Create a reader for the argument file name
                    val reader = new FileReader (args (0))
                    // Parse the file
                    parse (parser, reader) match {
                        // If it worked, we get a source tree
                        case Success (sourcetree, _) =>
                            // Pretty print the source tree
                            // emitter.emitln (pretty (product (sourcetree)))
                            // Process the program tree
                            if (processprogram (sourcetree)) {
                                // Enter read-eval-print-loop
                                emitter.emitln
                                emitter.emitln ("Prolog interpreter (exit with end of file: ^Z on Windows, ^D on Mac, Linux, Unix)")
                                emitter.emitln
                                super.main (args)
                            }
                        // Parsing failed, so report it
                        case f =>
                            emitter.emitln (f)
                    }
                } catch {
                    case e : FileNotFoundException =>
                        emitter.emitln (e.getMessage)
                }
            // Complain otherwise
            case _ =>
                emitter.emitln ("usage: run file.pl")

        }

    }

    /**
     * If the program is correct, this is the tree representing it.
     * Needed so that the process method can access it.
     */
    var programtree : Program = _

    /**
     * Process the program by analysing it to check for semantic
     * errors.  If any messages are produced, print them and
     * return false.  Otherwise, save the tree for the interpreter
     * and return true.
     */
    def processprogram (tree : Program) : Boolean = {
        resetmessages
        initTree (tree)
        check (tree)
        if (messagecount > 0) {
            report (new Emitter)
            false
        } else {
            programtree = tree
            true
        }
    }

    /**
     * The parser to use to parse each line of interactive input. We will
     * read queries, which are just literals followed by a period.
     */
    val start = query

    /**
     * The prompt to print before each line of input is read.
     */
    override val prompt = "?- "

    /**
     * Process a query by passing it and the program to the interpreter.
     */
    def process (querytree : Literal) {
        interpret (querytree, programtree, new Emitter)
    }

}
