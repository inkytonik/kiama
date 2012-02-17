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
package example.picojava

object Main extends Parser {

    import java.io.Reader
    import AbstractSyntax.Program
    import ErrorCheck._
    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.IO.filereader

    def main (args : Array[String]) : Unit = {
        for (filename <- args) {
            val reader = filereader (filename)
            val program = run (reader)
            initTree (program)
            val messages = program->errors
            for (msg <- messages)
                println (filename + ":" + msg)
        }
    }

    def run (in : Reader) : Program =
        parseAll (program, in) match {
            case Success (r, _) => r
            case f              => sys.error (f.toString)
        }

}
