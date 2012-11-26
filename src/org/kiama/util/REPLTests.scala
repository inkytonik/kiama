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

/**
 * A REPL that uses ScalaCheck to generate random instances of abstract
 * syntax trees of type T and prints them using a configurable emitter.
 */
trait GeneratingREPLBase[T] extends REPL {

    import org.scalacheck._

    /**
     * Don't ignore whitespace lines since we are using them to indicate
     * the generation command.
     */
    override val ignoreWhitespaceLines = false

    /**
     * Carry out setup processing for the REPL.
     */
    override def setup (args : Array[String]) : Boolean = {
        emitter.emitln ("Each time you hit ENTER a new instance is generated and printed.")
        true
    }

    /**
     * Display a prompt.
     */
    override def prompt () : String =
        "Hit ENTER to generate an instance: "

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
            case None     => emitter.emitln ("can't generate an instance")
        }
    }

    /**
     * Process a generated value.  Default: print it.
     */
    def process (t : T) {
        emitter.emitln (t)
    }

}

/**
 * A REPL that uses ScalaCheck to generate random instances of abstract
 * syntax trees of type T and prints them to standard output.
 */
trait GeneratingREPL[T] extends GeneratingREPLBase[T] with StdoutEmitter
