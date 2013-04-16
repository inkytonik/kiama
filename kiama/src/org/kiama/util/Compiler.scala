/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2013 Anthony M Sloane, Macquarie University.
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
      * Check the supplied arguments, returning any arguments that are to be
      * processed elsewhere. Default: return the arguments unchanged.
      */
    def checkargs (args : Array[String], emitter : Emitter) : Array[String] =
        args

    /**
     * The character encoding of input files read by this compiler.
     * Defaults to UTF-8.
     */
    def encoding : String =
        "UTF-8"

    /**
     * Process the arguments, using the given console for input and the
     * given emitter for output.  The arguments are first processed by
     * checkargs.  Any remaining arguments are interpreted as names of
     * files which are processed in turn by using `makeast` to turn
     * their contents into abstract syntax trees (ASTs) and then by
     * process which conducts arbitrary processing on the ASTs. The
     * character encoding of the files is given by the `encoding`
     * method.
     */
    def driver (args : Array[String], console : Console, emitter : Emitter) {
        val newargs = checkargs (args, emitter)
        for (arg <- newargs) {
            try {
                val reader = filereader (arg, encoding)
                makeast (reader, arg, emitter) match {
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
     * `Left`.  Returns `Right` with an error message if an AST cannot be made.
     */
    def makeast (reader : Reader, filename : String, emitter : Emitter) : Either[T,String]

    /**
     * Function to process the input that was parsed.  `console` should be
     * used to read anything needed by the processing.  `emitter` should be
     * used for output.  Return true if everything worked, false otherwise.
     * If false is returned, messages about the problem should be logged
     * by `process` using the messaging facility.
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
 * A compiler that uses a Scala combinator character-level parser. Define
 * `parser` to specify the actual parser to be used.
 */
trait RegexCompiler[T] extends CompilerBase[T] with RegexParsers {

    /**
     * The actual parser used to produce the AST.
     */
    def parser : Parser[T]

    def makeast (reader : Reader, filename : String, emitter : Emitter) : Either[T,String] =
        parseAll (parser, reader) match {
            case Success (ast, _) =>
                Left (ast)
            case f =>
                Right (f.toString)
        }

}

/**
 * A compiler that uses RegexParser to produce Attributable ASTs. The AST
 * is initialised with `initTree` by `process`. Override it and call it before
 * performing specific attribution.
 */
trait Compiler[T <: Attributable] extends RegexCompiler[T] {

    import org.kiama.attribution.Attribution.initTree

    def process (ast : T, console : Console, emitter : Emitter) : Boolean = {
        initTree (ast)
        true
    }

}

/**
 * A compiler that is capable of producing profiling reports. This trait
 * augments the argument processing to allow a leading `-p` option to
 * specify the profiling dimensions.
 */
trait ProfilingCompiler[T <: Attributable] extends Compiler[T] with Profiler {

    /**
     * Wrap the normal compiler driver in a `profile` call, if we are profiling
     * (indicated by a `-p` option with dimensions). Before calling the driver,
     * extract the comma-separated profiling dimensions if they are there. If
     * dimensions are present, print reports along those dimensions, otherwise
     * enter an interactive shell to allow reports to be produced.
     *
     * If not profiling, check for a `-t` option and, if present, run the compiler
     * driver to collect timings.
     *
     * If neither `-p` nor `-t` are the first option, just run the compiler driver
     * as normal.
     */
    override def driver (args : Array[String], console : Console, emitter : Emitter) {
        if ((args.size > 0) && (args (0).startsWith ("-p"))) {
            val dimensions = parseProfileOption (args (0).drop (2))
            profile (super.driver (args.tail, console, emitter), dimensions : _*)
        } else if ((args.size > 0) && (args (0) == "-t"))
            time (super.driver (args.tail, console, emitter))
        else
            super.driver (args, console, emitter)
    }

}
