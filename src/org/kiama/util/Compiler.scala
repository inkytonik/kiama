/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2011 Anthony M Sloane, Macquarie University.
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
package util

import java.io.Reader
import org.kiama.attribution.Attributable
import scala.util.parsing.combinator.RegexParsers

/**
 * Trait to provide basic functionality for a compiler-like program
 * constructed from phases.
 */
trait CompilerBase[T] {

    import java.io.Reader
    import org.kiama.util.Console
    import org.kiama.util.JLineConsole
    import org.kiama.util.Emitter
    import org.kiama.util.IO._
    import org.kiama.util.StringEmitter
    import scala.io.Source

    /**
     * Process the program in the file given as the first command-line
     * argument, read input using JLine input editing, and emit output
     * to the standard output.
     */
    def main (args : Array[String]) {
        driver (args, JLineConsole, new Emitter)
    }

    /**
     * Process the command-line arguments.  Returns the arguments that
     * have not been processed.  Output should be emitted using the
     * provided Emitter.  Default: do no processing.
     */
    def checkargs (args : Array[String], emitter : Emitter) : Array[String] =
        args

    /**
     * Process the arguments, using the given console for input and the
     * given emitter for output.  The arguments are first processed by
     * checkargs.  Any remaining arguments are interpreted as names of
     * UTF-8 encoded files which are processed in turn by using makeast to
     * turn their contents into abstract syntax trees (ASTs) and then by
     * process which conducts arbitrary processing on the ASTs.
     */
    def driver (args : Array[String], console : Console, emitter : Emitter) {
        val newargs = checkargs (args, emitter)
        for (arg <- newargs) {
            try {
                val reader = filereader (newargs (0))
                makeast (reader, newargs (0), emitter) match {
                    case Left (ast) =>
                        process (ast, console, emitter)
                    case Right (msg) =>
                        emitter.emitln (msg)
                }
            } catch {
                case e : FileNotFoundException =>
                    emitter.emitln (e.getMessage)
            }
        }
    }

    /**
     * Make an AST from the file with the given name, returning it wrapped in
     * Left.  Returns Right with an error message if an AST cannot be made.
     */
    def makeast (reader : Reader, filename : String, emitter : Emitter) : Either[T,String]

    /**
     * Function to process the input that was parsed.  console should be
     * used to read anything needed by the processing.  emitter should be
     * used for output.  Return true if everything worked, false otherwise.
     * If false is returned, messages about the problem should be logged
     * by process using the messaging facility.
     */
    def process (ast : T, console : Console, emitter : Emitter) : Boolean

    /**
     * Run the driver using the given args and return the resulting output,
     * which may be error messages or the result of running the compiled
     * program, for example. Read standard input from the specified console.
     * Reset the message buffer before calling the driver.
     */
    def compile (args : Array[String], console : Console) : String = {
        val emitter = new StringEmitter
        Messaging.resetmessages
        driver (args, console, emitter)
        emitter.result
    }

}

/**
 * A compiler that uses a Scala combinator character-level parser.
 */
trait RegexCompiler[T] extends CompilerBase[T] {

    this : RegexParsers =>

    /**
     * The actual parser used to produce the AST.
     */
    def parser : Parser[T]

    /**
     * Make an AST from the file with the given name by parsing it with
     * the parser.  If everything is ok, return the AST wrapped in Left,
     * otherwise return the error message wrapped in Right.
     */
    def makeast (reader : Reader, filename : String, emitter : Emitter) : Either[T,String] =
        parseAll (parser, reader) match {
            case Success (ast, _) =>
                Left (ast)
            case f =>
                Right (f.toString)
        }

}

/**
 * A compiler that uses RegexParser to produce Attributable ASTs.
 */
trait Compiler[T <: Attributable] extends RegexCompiler[T] {
 
    this : RegexParsers =>

    import org.kiama.attribution.Attribution.initTree

    /**
     * Function to process the input that was parsed.  By default, just
     * initialise the tree to get things like node properties.
     */
    def process (ast : T, console : Console, emitter : Emitter) : Boolean = {
        initTree (ast)
        true
    }

}
