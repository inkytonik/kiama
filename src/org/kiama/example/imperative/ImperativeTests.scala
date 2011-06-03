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
package example.imperative

import org.junit.runner.RunWith
import org.kiama.util.Tests
import org.scalatest.junit.JUnitRunner

/**
 * Imperative language tests pretty-printer tests.
 * Quite a few of the tests of other modules also use the imperative
 * language.
 */
@RunWith(classOf[JUnitRunner])
class ImperativeTests extends Tests {

    import AST._
    import PrettyPrinter._
    
    test ("pretty-print imperative variable") {
        expect ("xyz123") (pretty (Var ("xyz123")))
    }

    test ("pretty-print imperative variable - product") {
        expect ("""Var ("xyz123")""") (pretty (product (Var ("xyz123"))))
    }

    test ("pretty-print imperative assignment") {
        expect ("i = (0.0 * j);") (
            pretty (Asgn (Var ("i"), Mul (Num (0), Var ("j"))))
        )
    }

    test ("pretty-print imperative assignment - product") {
        expect ("""Asgn (Var ("i"), Mul (Num (0.0), Var ("j")))""") (
            pretty (product (Asgn (Var ("i"), Mul (Num (0), Var ("j")))))
        )
    }

    // { i = 10; count = 0; while (i) { count = count + 1; i = 1 + i; } }
    val p = 
        Seqn (List (
            Asgn (Var ("i"), Num (10)),
            Asgn (Var ("count"), Num (0)),
            While (Var ("i"),
                Seqn (List (
                    Asgn (Var ("count"), Add (Var ("count"), Num (1))),
                    Asgn (Var ("i"), Add (Num (1), Var ("i"))))))))
    
    val pp =
        """{
          |    i = 10.0;
          |    count = 0.0;
          |    while (i)
          |        {
          |            count = (count + 1.0);
          |            i = (1.0 + i);
          |        }
          |}""".stripMargin
    
    val ppp =
        """Seqn (
          |    List (
          |        Asgn (Var ("i"), Num (10.0)),
          |        Asgn (Var ("count"), Num (0.0)),
          |        While (
          |            Var ("i"),
          |            Seqn (
          |                List (
          |                    Asgn (
          |                        Var ("count"),
          |                        Add (Var ("count"), Num (1.0))),
          |                    Asgn (Var ("i"), Add (Num (1.0), Var ("i"))))))))""".stripMargin

    test ("pretty-print non-trivial imperative program") {
        expect (pp) (pretty (p)) 
    }

    test ("pretty-print non-trivial imperative program (product)") {
        expect (ppp) (pretty (product (p))) 
    }
    
}
