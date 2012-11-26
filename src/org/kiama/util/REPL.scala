/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2012 Anthony M Sloane, Macquarie University.
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

import scala.util.parsing.combinator.RegexParsers

/**
 * General support for applications that implement read-eval-print loops (REPLs).
 * Output is emitted using a configurable emitter.
 */
trait REPLBase {

    /**
     * The emitter to use to display any output.
     */
    def emitter : Emitter

    /**
     * Whether lines consisting entirely of whitespace should be ignored or not.
     * Default: yes.
     */
    def ignoreWhitespaceLines : Boolean =
        true

    /**
     * Read lines from the console and pass non-null ones to `processline`.
     * If `ignoreWhitespaceLines` is true, do not pass lines that contain
     * just whitespace, otherwise do. Continue until `processline` returns
     * false. Call `setup` before entering the loop and call `prompt` each
     * time input is about to be read.  The command-line arguments are
     * passed to the `setup` method.
     */
    def main (args : Array[String]) {

        // If the setup works, read lines and process them
        if (setup (args)) {
            var cont = true
            while (cont) {
                val line = JLineConsole.readLine (prompt)
                if (line == null) {
                    emitter.emitln
                    cont = false
                } else if (!ignoreWhitespaceLines || (line.trim.length != 0))
                    processline (line)
            }
        }

    }

    /**
     * Carry out setup processing for the REPL.  Default: do nothing.
     */
    def setup (args : Array[String]) : Boolean =
        true

    /**
     * Define the prompt (default: `"> "`).
     */
    def prompt () : String =
        "> "

    /**
     * Process a user input line.
     */
    def processline (line : String) : Unit

}

/**
 * General support for applications that implement read-eval-print loops (REPLs).
 * Output is emitted to standard output.
 */
trait REPL extends REPLBase with StdoutEmitter

/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. Output is emitted using a configurable emitter.
 */
trait ParsingREPLBase[T] extends REPLBase with RegexParsers {

    /**
     * Process a user input line by parsing it to get a value of type `T`,
     * then passing it to the `process` method.
     */
    def processline (line : String) {
        parseAll (start, line) match {
            case Success (e, in) if in.atEnd =>
                process (e)
            case Success (_, in) =>
                emitter.emitln ("extraneous input at " + in.pos)
            case f =>
                emitter.emitln (f)
        }
    }

    /**
     * The parser to use to convert user input lines into values.
     */
    def start : Parser[T]

    /**
     * Process a user input value.
     */
    def process (t : T) : Unit

}

/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. Output is emitted to standard output.
 */
trait ParsingREPL[T] extends ParsingREPLBase[T] with StdoutEmitter
