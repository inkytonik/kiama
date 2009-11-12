/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2009 Anthony M Sloane, Macquarie University.
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

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava.tests

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite

class ErrorTests extends JUnit3Suite {

    import java.io.FileReader
    import kiama.example.picojava.ErrorCheck._
    import kiama.example.picojava.Parser._

    /**
     * Parse the illegal program and make sure that the errors and their
     * positions are as expected.
     */
    def testErrorInfo {
        val text = """
{
  class A extends B{
    boolean a;
    a = b;
    A refA;
    a = refA;
  }
  class B extends A {
  }
  class C {
  }
  class D {
  }
  C refC;
  D refD;
  refC = refD;
}
""";
        parseAll (program, text) match {
            case Success (ast, _) => {
                val messages = ast->errors
                assertEquals ("5.9: Unknown identifier b", messages (0))
                assertEquals ("7.5: Can not assign a variable of type boolean to a value of type A", messages (1))
                assertEquals ("3.3: Cyclic inheritance chain for class A", messages (2))
                assertEquals ("9.3: Cyclic inheritance chain for class B", messages (3))
                assertEquals ("17.3: Can not assign a variable of type C to a value of type D", messages (4))
            }
            case f : Failure => fail (f.toString)
        }
    }

}
