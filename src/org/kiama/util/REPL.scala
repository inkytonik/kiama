/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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
 */
trait REPL {

    /**
     * Read lines from the console and pass non-null ones to processline.
     * Continue until processline returns false. The command-line arguments
     * are passed to the setup function.  Calls setup before entering the
     * loop and prompt each time input is about to be read.
     */
    def main (args : Array[String]) {

        // If the setup works, read lines and process them
        if (setup (args)) {
            while (true) {
                val line = JLineConsole.readLine (prompt)
                if (line == null) {
                    println
                    return
                } else
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
     * Define the prompt (default: "> ").
     */
    def prompt () = "> "

    /**
     * Process a user input line.
     */
    def processline (line : String)

}

/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them.
 */
trait ParsingREPL[T] extends REPL with RegexParsers {

    /**
     * Process a user input line by parsing it to get a value of type T,
     * then passing it to the type-specific process.
     */
    def processline (line : String) {
        parseAll (start, line) match {
            case Success (e, in) if in.atEnd =>
                process (e)
            case Success (_, in) =>
                println ("extraneous input at " + in.pos)
            case f =>
                println (f)
        }
    }

    /**
     * The parser to use to convert user input lines into values.
     */
    def start : Parser[T]

    /**
     * Process a user input value.
     */
    def process (t : T)

}

/**
 * A REPL that uses ScalaCheck to generate random instances of abstract
 * syntax trees of type T and prints them.
 */
trait GeneratingREPL[T] extends REPL {

    import org.scalacheck._

    /**
     * Carry out setup processing for the REPL.
     */
    override def setup (args : Array[String]) : Boolean = {
        println ("Each time you hit ENTER a new instance is generated and printed.")
        true
    }

    /**
     * Display a prompt.
     */
    override def prompt () = "Hit ENTER to generate an instance: "

    /**
     * The generator to use to make values of type T.
     */
    def generator : Arbitrary[T]

    /**
     * Generate a new instance and print it, ignoring the input line.
     */
    def processline (line : String) {
        generator.arbitrary (Gen.Params ()) match {
            case Some (t) => process (t)
            case None     => println ("can't generate an instance")
        }
    }

    /**
     * Process a generated value.  Default: print it.
     */
    def process (t : T) {
        println (t)
    }

}
