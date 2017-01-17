/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2017 Anthony M Sloane, Macquarie University.
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
package example.dataflow

import DataflowTree.Stm
import org.bitbucket.inkytonik.kiama.util.TestCompiler

/**
 * Tests of data flow attribution.
 */
class DataflowTests extends Driver with TestCompiler[Stm] {

    import DataflowTree._

    /*
     * {                     (prog)
     *     y = v             (s1)
     *     z = y             (s2)
     *     x = v             (s3)
     *     while (x) {       (s4, s41)
     *         x = w         (s411)
     *         x = v         (s412)
     *     }
     *     return x          (s5)
     * }
     */
    val s1 = Assign("y", "v")
    val s2 = Assign("z", "y")
    val s3 = Assign("x", "v")
    val s411 = Assign("x", "w")
    val s412 = Assign("x", "v")
    val s41 = Block(List(s411, s412))
    val s4 = While("x", s41)
    val s5 = Return("x")
    val prog = Block(List(s1, s2, s3, s4, s5))

    val tree = new DataflowTree(prog)
    val dataflow = new Dataflow(tree)
    import dataflow._

    val outAttr = out.asInstanceOf[CircularAttribute[Stm, Set[Var]]]

    test("succ - s1") {
        succ(s1) should beSameCollectionAs(Set(s2))
    }

    test("succ - s2") {
        succ(s2) should beSameCollectionAs(Set(s3))
    }

    test("succ - s3") {
        succ(s3) should beSameCollectionAs(Set(s4))
    }

    test("succ - s4") {
        succ(s4) should beSameCollectionAs(Set(s5, s41))
    }

    test("succ - s41") {
        succ(s41) should beSameCollectionAs(Set(s411))
    }

    test("succ - s411") {
        succ(s411) should beSameCollectionAs(Set(s412))
    }

    test("succ - s412") {
        succ(s412) should beSameCollectionAs(Set(s4))
    }

    test("succ - s5") {
        succ(s5) shouldBe Set()
    }

    test("in - s1") {
        in(s1) shouldBe Set("w", "v")
    }

    test("in - s2") {
        in(s2) shouldBe Set("y", "w", "v")
    }

    test("in - s3") {
        in(s3) shouldBe Set("w", "v")
    }

    test("in - s4") {
        in(s4) shouldBe Set("x", "w", "v")
    }

    test("in - s411") {
        in(s411) shouldBe Set("w", "v")
    }

    test("in - s412") {
        in(s412) shouldBe Set("w", "v")
    }

    test("in - s5") {
        in(s5) shouldBe Set("x")
    }

    test("out - s1") {
        out(s1) shouldBe Set("y", "w", "v")
    }

    test("out - s2") {
        out(s2) shouldBe Set("w", "v")
    }

    test("out - s3") {
        out(s3) shouldBe Set("x", "w", "v")
    }

    test("out - s4") {
        out(s4) shouldBe Set("x", "w", "v")
    }

    test("out - s411") {
        out(s411) shouldBe Set("w", "v")
    }

    test("out - s412") {
        out(s412) shouldBe Set("x", "w", "v")
    }

    test("out - s412 (reset)") {
        outAttr.reset()
        outAttr.hasBeenComputedAt(s412) shouldBe false
        outAttr(s412) shouldBe Set("x", "w", "v")
        outAttr.hasBeenComputedAt(s412) shouldBe true
        outAttr.reset()
        outAttr.hasBeenComputedAt(s412) shouldBe false
    }

    test("out - s5") {
        out(s5) shouldBe Set()
    }

    filetests("Dataflow", "example/dataflow/tests", ".data", ".out")
    filetests("Dataflow", "example/dataflow/tests", ".dataerr", ".err")

}
