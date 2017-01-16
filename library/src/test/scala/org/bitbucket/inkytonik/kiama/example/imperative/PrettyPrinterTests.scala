/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.imperative

import ImperativeTree._
import org.bitbucket.inkytonik.kiama.util

/**
 * Imperative language pretty-printer tests.
 */
class PrettyPrinterTests extends PrettyPrinter with util.PrettyPrinterTests {

    test("pretty-print imperative variable") {
        format(Var("xyz123")).layout shouldBe "xyz123"
    }

    test("pretty-print imperative variable - product") {
        pretty(any(Var("xyz123"))).layout shouldBe """Var ("xyz123")"""
    }

    test("pretty-print imperative assignment") {
        format(Asgn(Var("i"), Mul(Num(0), Var("j")))).layout shouldBe "i = (0.0 * j);"
    }

    test("pretty-print imperative assignment - product") {
        pretty(any(Asgn(Var("i"), Mul(Num(0), Var("j"))))).layout shouldBe """Asgn (Var ("i"), Mul (Num (0.0), Var ("j")))"""
    }

    // { i = 10; count = 0; while (i) { count = count + 1; i = 1 + i; } }
    val p =
        Seqn(Vector(
            Asgn(Var("i"), Num(10)),
            Asgn(Var("count"), Num(0)),
            While(
                Var("i"),
                Seqn(Vector(
                    Asgn(Var("count"), Add(Var("count"), Num(1))),
                    Asgn(Var("i"), Add(Num(1), Var("i")))
                ))
            )
        ))

    val pp1 =
        """{
          |    i = 10.0;
          |    count = 0.0;
          |    while (i) { count = (count + 1.0); i = (1.0 + i); }
          |}""".stripMargin

    val pp2 =
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
          |    Vector (
          |        Asgn (Var ("i"), Num (10.0)),
          |        Asgn (Var ("count"), Num (0.0)),
          |        While (
          |            Var ("i"),
          |            Seqn (
          |                Vector (
          |                    Asgn (Var ("count"), Add (Var ("count"), Num (1.0))),
          |                    Asgn (Var ("i"), Add (Num (1.0), Var ("i"))))))))""".stripMargin

    test("pretty-print non-trivial imperative program (default width)") {
        format(p).layout shouldBe pp1
    }

    test("pretty-print non-trivial imperative program (narrow)") {
        pretty(group(toDoc(p)), 40).layout shouldBe pp2
    }

    test("pretty-print non-trivial imperative program (product)") {
        pretty(any(p)).layout shouldBe ppp
    }

}
