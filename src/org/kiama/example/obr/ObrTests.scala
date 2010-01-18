/**
 * Obr language implementation main program.
 *
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

package org.kiama.example.obr

import org.scalatest.FunSuite
import org.kiama.util.Testing

/**
 * Obr compiler tests.
 */
class ObrTests extends FunSuite with Testing {

    /**
    * An emitter that records the code in a string that can be accessed
    * via the result method.
    */
    class StringEmitter extends Emitter {
        val b = new StringBuilder
        override def emit (line : String) = b.append (line + "\n")
        def result () = b.result ()
    }

    /**
     * Compile the Obr program in the file given as the argument and return
     * the code or None if compilation failed.
     */
    def compile (filename : String) : Option[String] = {
        val emitter = new StringEmitter
        if (Main.driver (Array (filename), emitter)) {
            Some (emitter.result ())
        } else {
            None
        }
    }

    test ("Obr compiler generates correct code") {
        filetests ("src/org/kiama/example/obr/tests", ".obr", ".s", compile)
    }

}
