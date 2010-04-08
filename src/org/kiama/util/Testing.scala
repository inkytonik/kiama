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

import org.scalatest.FunSuite

/**
 * Class of objects that can emit code.  By default, code is output to the
 * console.  Subclass this if you need it to go somewhere else.
 */
class Emitter {

    /**
     * Emit anything.
     */
    def emit (any : Any) {
        print (any.toString)
    }

    /**
     * Emit anything and start a new line.
     */
    def emitln (any : Any) {
        println (any.toString)
    }

}

/**
 * Trait to supply useful testing infrastructure.
 */
trait Testing {

    self : Compiler[_] with FunSuite =>

    import org.scalatest.TestFailedException

    /**
    * An emitter that records the code in a string that can be accessed
    * via the result method.
    */
    private class StringEmitter extends Emitter {
        val b = new StringBuilder
        override def emit (any : Any) = b.append (any.toString)
        override def emitln (any : Any) = b.append (any.toString).append('\n')
        def result () = b.result ()
    }

    /**
     * Compile the program in the file given as the argument and return
     * the code or None if compilation failed.
     */
    private def compile (filename : String) : Option[String] = {
        val emitter = new StringEmitter
        Messaging.resetmessages
        if (driver (Array (filename), emitter)) {
            Some (emitter.result ())
        } else {
            None
        }
    }

    /**
     * Run tests that process files in path.  All files whose names end
     * in srcext are processed.  Processing is done by the function process
     * which must return either Some (s) where s is the output or None if
     * processing failed.  If srcext is .x an resext is .y, then the
     * expected result for foo.x is found in file foo.y.  A test fails if
     * either the processing fails or it succeeds with the wrong result.
     */
    def filetests (path : String, srcext : String, resext : String,
                   process : String => Option[String] = compile) {

        import java.io.File
        import scala.io.Source

        val dir = new File (path)
        val children = dir.list ()
        if (children == null) {
            fail ("no file tests found at " + path)
        } else {
            for (c <- children) {
                if (c.endsWith (srcext)) {
                    val cp = path + "/" + c
                    val res =
                        try {
                            process (cp)
                        } catch {
                            case e : Exception =>
                                info (cp + " failed with an exception ")
                                throw (e)
                        }
                    res match {
                        case Some (cc) =>
                            val rp = cp.replace (srcext, resext)
                            val rc = Source.fromPath (rp).mkString
                            if (cc != rc)
                                fail (cp + " generated bad output:\n" + cc +
                                     "expected:\n" + rc)
                        case None =>
                            fail (cp + " did not compile")
                    }
                }
            }
        }

    }

}
