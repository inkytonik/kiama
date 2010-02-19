/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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

package org.kiama.util

/**
 * Trait to provide basic functionality for a compiler-like program that
 * parses an input file and processes it in some way, possibly emitting
 * some output.  T is the type of the AST that results from parsing.
 */
trait Compiler[T] {

    import java.io.FileReader
    import org.kiama.util.Messaging._
    import org.kiama.util.Emitter

    /**
     * Compile the program in the file given as the first command-line
     * argument and emit output to the console.
     */
    def main (args : Array[String]) {
        driver (args, new Emitter)
    }

    /**
     * Compile the program in the file given as the first command-line
     * argument.  Output is produced using the specified emitter.  True is
     * returned if it all worked, otherwise false.
     */
    def driver (args : Array[String], emitter : Emitter) : Boolean =
        args.size match {
            case 1 =>
                parse (args (0)) match {
                    case Some (ast) =>
                        process (ast, emitter)
                    case None =>
                        false
                }
            case _ =>
                println (usage)
                false
        }

    /**
     * The usage message for an erroneous invocation.
     */
    val usage : String

    /**
     * The parser to use to process the input file into an AST.
     */
    def parse (filename : String) : Option[T]

    /**
     * Function to process the input that was parsed.  emitter is
     * used for output.  Return true if everything worked, false
     * otherwise.
     */
    def process (ast : T, emitter : Emitter) : Boolean

}
