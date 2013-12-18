/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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

import org.kiama.attribution.Attributable
import scala.collection.immutable.Seq
import scala.util.parsing.combinator.RegexParsers

/**
 * General support for applications that implement read-eval-print loops (REPLs).
 */
trait REPLBase[C <: REPLConfig] extends Profiler {

    import scala.annotation.tailrec

    /**
     * Banner message that is printed before the REPL starts.
     */
    def banner : String

    /**
     * The entry point for this REPL.
     */
    def main (args : Array[String]) {
        driver (args.toIndexedSeq)
    }

    /**
     * Create the configuration for a particular run of the REPL. If supplied, use
     * `emitter` instead of a standard output emitter.
     */
    def createConfig (args : Seq[String], emitter : Emitter = new Emitter) : C

    /**
     * Driver for this REPL. First, use the argument list to create a
     * configuration for this execution. If the arguments parse ok, then
     * print the REPL banner. Read lines from the console and pass non-null ones
     * to `processline`. If `ignoreWhitespaceLines` is true, do not pass lines that
     * contain just whitespace, otherwise do. Continue until `processline`
     * returns false. Call `prompt` each time input is about to be read.
     */
    def driver (args : Seq[String]) {
        val config = createConfig (args)
        config.emitter.emitln (banner)
        if (config.profile.get != None) {
            val dimensions = parseProfileOption (config.profile ())
            profile (processlines (config), dimensions, config.logging ())
        } else if (config.time ())
            time (processlines (config))
        else
            processlines (config)
    }

    /**
     * Define the prompt (default: `"> "`).
     */
    def prompt : String =
        "> "

    /**
     * Process interactively entered lines, one by one, until end of file.
     */
    @tailrec
    final def processlines (config : C) {
        val line = config.console ().readLine (prompt)
        if (line == null) {
            config.emitter.emitln
        } else if (config.processWhitespaceLines () || (line.trim.length != 0))
            processlines (processline (line, config))
    }

    /**
     * Process a user input line. The return value allows the processing to
     * return a new configuration that will be used in subsequent processing.
     */
    def processline (line : String, config : C) : C

}

/**
 * General support for applications that implement read-eval-print loops (REPLs).
 */
trait REPL extends REPLBase[REPLConfig] {

    def createConfig (args : Seq[String], emitter : Emitter = new Emitter) : REPLConfig =
        new REPLConfig (args, emitter)

}

/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. Output is emitted using a configurable emitter.
 */
trait ParsingREPLBase[T <: Attributable, C <: REPLConfig] extends REPLBase[C] with RegexParsers {

    import org.kiama.attribution.Attribution.initTree

    /**
     * Process a user input line by parsing it to get a value of type `T`,
     * then passing it to the `process` method. Returns the configuration
     * unchanged.
     */
    def processline (line : String, config : C) : C = {
        parseAll (parser, line) match {
            case Success (e, in) if in.atEnd =>
                process (e, config)
            case Success (_, in) =>
                config.emitter.emitln (s"extraneous input at ${in.pos}")
            case f =>
                config.emitter.emitln (f)
        }
        config
    }

    /**
     * The parser to use to convert user input lines into values.
     */
    def parser : Parser[T]

    /**
     * Process a user input value. By default, just initialise the tree.
     */
    def process (t : T, config : C) {
        initTree (t)
    }

}

/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. `C` is the type of the configuration.
 */
trait ParsingREPLWithConfig[T <: Attributable, C <: REPLConfig] extends ParsingREPLBase[T,C]

/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. Output is emitted to standard output.
 */
trait ParsingREPL[T <: Attributable] extends ParsingREPLWithConfig[T,REPLConfig] {

    def createConfig (args : Seq[String], emitter : Emitter = new Emitter) : REPLConfig =
        new REPLConfig (args, emitter)

}
