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
package example.til

import java.io.Reader
import org.kiama.util.PositionedParserUtilities

/**
 * Standard main program for TIL chairmarks.
 */
trait Main {

    import org.kiama.util.Emitter
    import org.kiama.util.IO.{filereader, FileNotFoundException}

    val emitter = new Emitter

    /**
     * Accept file name arguments and process them one-by-one by
     * passing a reader on the file to process.  The resulting
     * value is printed.
     */
    def main (args : Array[String]) {
        for (arg <- args) {
            try {
                val reader = filereader (arg)
                val result = process (reader)
                emitter.emitln (result)
            } catch {
                case e : FileNotFoundException =>
                    emitter.emitln (s"can't open $arg for reading")
            }
        }
    }

    /**
      * Process the file given by the argument reader and return
      * some useful result.
      */
    def process (reader : Reader) : Any

}

/**
 * Main program for TIL chairmarks that just parse.
 */
trait ParsingMain extends Main {

    self : PositionedParserUtilities =>

    /**
     * The parser to call.
     */
    def parser : Parser[AST.Program]

    /**
      * Process the file given by the argument reader and return
      * some useful result. This default implementation parses
      * the input and returns the parser result.
      */
    def process (reader : Reader) : Any =
        parseAll (parser, reader) match {
            case Success (p, _) => p
            case f              => f
        }

}

/**
 * Standard main program for TIL chairmarks that parse and transform.
 */
trait TransformingMain extends ParsingMain {

    self : PositionedParserUtilities =>

    import org.kiama.rewriting.Rewriter._

    /**
     * Transform a single AST.
     */
    def transform (ast : AST.Program) : AST.Program

    /**
      * Process the file given by the argument reader by parsing it,
      * transforming it and returning the resulting AST.
      */
    override def process (reader : Reader) : Any =
        parseAll (parser, reader) match {
            case Success (p, _) => transform (p)
            case f              => f
        }

}
