/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.kiama
package example.picojava.tests

import org.kiama.util.Tests

class BasicNameResolutionTests extends Tests {

    import org.kiama.attribution.Attribution.initTree
    import org.kiama.example.picojava.NameResolution._
    import org.kiama.example.picojava.PicoJavaTree._
    import org.kiama.example.picojava.TypeAnalyser._
    import scala.collection.immutable.Seq

    // For the actual program text, see BasicNameResolutionTests.pj

    val declRx = VarDecl (Use ("int"), "x")
    val xInR   = Use ("x")
    val declRz = VarDecl (Use ("int"), "z")
    val zInR   = Use ("z")
    val yInR   = Use ("y")
    val yInA   = Use("y")
    val xInA   = Use ("x")
    val declAz = VarDecl (Use ("int"), "z")
    val zInA   = Use ("z")

    val ast =
        Program (Block (
            Seq (declRx,
                 AssignStmt (xInR, zInR),
                 declRz,
                 AssignStmt (yInR, Use ("x")),
                 ClassDecl ("A", None, Block (
                     Seq (declAz,
                          AssignStmt (xInA, zInA),
                          AssignStmt (yInA, Use ("z"))))))))

    override def beforeAll () {
        initTree (ast)
    }

    test ("bindings at the same nesting level are resolved") {
        assertResult (declRx) (xInR->decl)
    }

    test ("bindings at an outer nesting level are resolved") {
        assertResult (declRx) (xInA->decl)
    }

    test ("names can be declared after use") {
        assertResult (declRz) (zInR->decl)
    }

    test ("a missing declaration for a top-level use is detected") {
        assert (isUnknown (yInR->decl))
    }

    test ("a missing declaration for a nested use is detected") {
        assert (isUnknown (yInA->decl))
    }

    test ("a local shadowing binding is resolved") {
        assertResult (declAz) (zInA->decl)
    }

}

