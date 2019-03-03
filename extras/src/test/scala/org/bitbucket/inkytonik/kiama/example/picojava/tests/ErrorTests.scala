/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.bitbucket.inkytonik.kiama
package example.picojava.tests

import org.bitbucket.inkytonik.kiama.example.picojava.SyntaxAnalyser
import org.bitbucket.inkytonik.kiama.util.ParseTests

class ErrorTests extends ParseTests {

    import java.util.ArrayList
    import org.bitbucket.inkytonik.kiama.example.picojava.ErrorCheck
    import org.bitbucket.inkytonik.kiama.example.picojava.PicoJavaTree.PicoJavaTree
    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
    import org.bitbucket.inkytonik.kiama.util.StringSource

    val parsers = new SyntaxAnalyser(positions)

    /**
     * Parse and evaluate a program string and return a collection of the
     * messages that result.
     */
    def eval(term : String) : ArrayList[String] = {
        parsers.parseAll(parsers.program, StringSource(term)) match {
            case Success(ast, _) =>
                val tree = new PicoJavaTree(ast)
                val analyser = new ErrorCheck(tree)
                analyser.errors
            case result : NoSuccess =>
                val messages = new ArrayList[String]
                messages.add(result.message)
                messages
        }
    }

    /**
     * Parse an illegal program and make sure that the errors and their
     * positions are as expected.
     */
    test("semantic errors are correctly reported") {

        val term = """
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

        val messages = eval(term)
        messages.size shouldBe 5
        messages.get(0) shouldBe "Unknown identifier b"
        messages.get(1) shouldBe "Can not assign a variable of type boolean to a value of type A"
        messages.get(2) shouldBe "Cyclic inheritance chain for class A"
        messages.get(3) shouldBe "Cyclic inheritance chain for class B"
        messages.get(4) shouldBe "Can not assign a variable of type C to a value of type D"

    }

}
