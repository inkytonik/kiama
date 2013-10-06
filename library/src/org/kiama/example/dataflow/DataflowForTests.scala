/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package example.dataflow

import org.kiama.util.Tests

/**
 * Tests of extended data flow attribution.
 */
class DataflowForTests extends Tests {

    import DataflowAST._
    import Dataflow._
    import org.kiama.attribution.Attribution._

    DataflowFor.setup ()
    DataflowForeach.setup ()

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
    val s1 = Assign ("y", "v")
    val s2 = Assign ("z", "y")
    val s3 = Assign ("x", "v")
    val s411 = Assign ("x", "w")
    val s412 = Assign ("x", "v")
    val s41 = Block (List (s411, s412))
    val s4 = Foreach ("x", s41)
    val s5 = Return ("x")
    val prog = Block (List (s1, s2, s3, s4, s5))
    initTree (prog)

    test ("in s1") {
        assertResult (Set ("w", "v")) (in (s1))
    }

    test ("in s2") {
        assertResult (Set ("y", "w", "v")) (in (s2))
    }

    test ("in s3") {
        assertResult (Set ("w", "v")) (in (s3))
    }

    test ("in s4") {
        assertResult (Set ("x", "w", "v")) (in (s4))
    }

    test ("in s411") {
        assertResult (Set ("w", "v")) (in (s411))
    }

    test ("in s412") {
        assertResult (Set ("w", "v")) (in (s412))
    }

    test ("in s5") {
        assertResult (Set ("x")) (in (s5))
    }

    test ("out s1") {
        assertResult (Set ("y", "w", "v")) (out (s1))
    }

    test ("out s2") {
        assertResult (Set ("w", "v")) (out (s2))
    }

    test ("out s3") {
        assertResult (Set ("x", "w", "v")) (out (s3))
    }

    test ("out s4") {
        assertResult (Set ("x", "w", "v")) (out (s4))
    }

    test ("out s411") {
        assertResult (Set ("w", "v")) (out (s411))
    }

    test ("out s412") {
        assertResult (Set ("x", "w", "v")) (out (s412))
    }

    test ("out s5") {
        assertResult (Set ()) (out (s5))
    }

}
