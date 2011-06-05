/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011 Anthony M Sloane, Macquarie University.
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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Basic tests of compiler module.  Normal usage is tested by many of
 * the examples.
 */
@RunWith(classOf[JUnitRunner])
class CompilerTests extends Compiler[Any] with Tests {

    import java.io.FileReader
    import org.scalatest.TestFailedException

    def makeast (reader : FileReader, filename : String) : Either[Any,String] =
         Right ("Dummy")

    def process (ast : Any, console : Console, emitter : Emitter) : Boolean =
         false

    test ("compiler driver produces an appropriate message if a file is not found") {
        val e = new StringEmitter
        driver (Array ("IDoNotExist.txt"), new StringConsole (""), e)
        val msg =
            if (System.getProperty("os.name").startsWith ("Windows"))
                "The system cannot find the file specified"
            else
                "No such file or directory"
        expect ("IDoNotExist.txt (" + msg + ")\n") (e.result)
    }
    
    test ("filetests using a directory that doesn't exist fails") {
        val i = intercept[IllegalArgumentException] {
                    filetests ("Compiler", "src/org/kiama/util/IDoNotExist", ".src", ".out")
                }
        expect ("bad test file path src/org/kiama/util/IDoNotExist") (i.getMessage)
    }

}
