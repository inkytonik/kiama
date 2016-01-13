/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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
        assertSameCollection(Set(s2))(succ(s1))
    }

    test("succ - s2") {
        assertSameCollection(Set(s3))(succ(s2))
    }

    test("succ - s3") {
        assertSameCollection(Set(s4))(succ(s3))
    }

    test("succ - s4") {
        assertSameCollection(Set(s5, s41))(succ(s4))
    }

    test("succ - s41") {
        assertSameCollection(Set(s411))(succ(s41))
    }

    test("succ - s411") {
        assertSameCollection(Set(s412))(succ(s411))
    }

    test("succ - s412") {
        assertSameCollection(Set(s4))(succ(s412))
    }

    test("succ - s5") {
        assertSameCollection(Set())(succ(s5))
    }

    test("in - s1") {
        assertResult(Set("w", "v"))(in(s1))
    }

    test("in - s2") {
        assertResult(Set("y", "w", "v"))(in(s2))
    }

    test("in - s3") {
        assertResult(Set("w", "v"))(in(s3))
    }

    test("in - s4") {
        assertResult(Set("x", "w", "v"))(in(s4))
    }

    test("in - s411") {
        assertResult(Set("w", "v"))(in(s411))
    }

    test("in - s412") {
        assertResult(Set("w", "v"))(in(s412))
    }

    test("in - s5") {
        assertResult(Set("x"))(in(s5))
    }

    test("out - s1") {
        assertResult(Set("y", "w", "v"))(out(s1))
    }

    test("out - s2") {
        assertResult(Set("w", "v"))(out(s2))
    }

    test("out - s3") {
        assertResult(Set("x", "w", "v"))(out(s3))
    }

    test("out - s4") {
        assertResult(Set("x", "w", "v"))(out(s4))
    }

    test("out - s411") {
        assertResult(Set("w", "v"))(out(s411))
    }

    test("out - s412") {
        assertResult(Set("x", "w", "v"))(out(s412))
    }

    test("out - s412 (reset)") {
        outAttr.reset()
        assert(!outAttr.hasBeenComputedAt(s412))
        assertResult(Set("x", "w", "v"))(outAttr(s412))
        assert(outAttr.hasBeenComputedAt(s412))
        outAttr.reset()
        assert(!outAttr.hasBeenComputedAt(s412))
    }

    test("out - s5") {
        assertResult(Set())(out(s5))
    }

    filetests("Dataflow", "src/org/bitbucket/inkytonik/kiama/example/dataflow/tests", ".data", ".out")
    filetests("Dataflow", "src/org/bitbucket/inkytonik/kiama/example/dataflow/tests", ".dataerr", ".err")

}
