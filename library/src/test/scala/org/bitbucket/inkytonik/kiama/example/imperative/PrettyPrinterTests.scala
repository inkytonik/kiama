/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
        pretty(any(Var("xyz123"))).layout shouldBe """Var("xyz123")"""
    }

    test("pretty-print imperative assignment") {
        format(Asgn(Var("i"), Mul(Num(0), Var("j")))).layout shouldBe "i = (0.0 * j);"
    }

    test("pretty-print imperative assignment - product") {
        pretty(any(Asgn(Var("i"), Mul(Num(0), Var("j"))))).layout shouldBe """Asgn(Var("i"), Mul(Num(0.0), Var("j")))"""
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
        """Seqn(
          |    Vector(
          |        Asgn(Var("i"), Num(10.0)),
          |        Asgn(Var("count"), Num(0.0)),
          |        While(
          |            Var("i"),
          |            Seqn(
          |                Vector(
          |                    Asgn(Var("count"), Add(Var("count"), Num(1.0))),
          |                    Asgn(Var("i"), Add(Num(1.0), Var("i"))))))))""".stripMargin

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
