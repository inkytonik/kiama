/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.dataflow

import org.bitbucket.inkytonik.kiama.util.KiamaTests

/**
 * Tests of extended data flow attribution.
 */
class DataflowForTests extends KiamaTests {

    import DataflowTree._

    /*
     * begin                 (prog)
     *     y = v             (s1)
     *     z = y             (s2)
     *     x = v             (s3)
     *     foreach (x) do    (s4, s41)
     *         x = w         (s411)
     *         x = v         (s412)
     *     end
     *     return x          (s5)
     * end
     */
    val s1 = Assign("y", "v")
    val s2 = Assign("z", "y")
    val s3 = Assign("x", "v")
    val s411 = Assign("x", "w")
    val s412 = Assign("x", "v")
    val s41 = Block(List(s411, s412))
    val s4 = Foreach("x", s41)
    val s5 = Return("x")
    val prog = Block(List(s1, s2, s3, s4, s5))

    val tree = new DataflowTree(prog)
    val dataflowfor = new DataflowFor(tree)
    import dataflowfor._

    override def beforeAll : Unit = {
        addForAndForeachCases()
    }

    test("in s1") {
        in(s1) shouldBe Set("w", "v")
    }

    test("in s2") {
        in(s2) shouldBe Set("y", "w", "v")
    }

    test("in s3") {
        in(s3) shouldBe Set("w", "v")
    }

    test("in s4") {
        in(s4) shouldBe Set("x", "w", "v")
    }

    test("in s411") {
        in(s411) shouldBe Set("w", "v")
    }

    test("in s412") {
        in(s412) shouldBe Set("w", "v")
    }

    test("in s5") {
        in(s5) shouldBe Set("x")
    }

    test("out s1") {
        out(s1) shouldBe Set("y", "w", "v")
    }

    test("out s2") {
        out(s2) shouldBe Set("w", "v")
    }

    test("out s3") {
        out(s3) shouldBe Set("x", "w", "v")
    }

    test("out s4") {
        out(s4) shouldBe Set("x", "w", "v")
    }

    test("out s411") {
        out(s411) shouldBe Set("w", "v")
    }

    test("out s412") {
        out(s412) shouldBe Set("x", "w", "v")
    }

    test("out s5") {
        out(s5) shouldBe Set()
    }

}
