/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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

package org.kiama
package example.picojava.tests

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BasicNameResolutionTests extends FunSuite {

    import org.kiama.example.picojava.AbstractSyntax._
    import org.kiama.example.picojava.NameResolution._
    import org.kiama.example.picojava.TypeAnalysis._

    // For the actual program text, see BasicNameResolutionTests.pj

    private val declRx = VarDecl ("x", Use ("int"))
    private val xInR   = Use ("x")
    private val declRz = VarDecl ("z", Use ("int"))
    private val zInR   = Use ("z")
    private val yInR   = Use ("y")
    private val yInA   = Use("y")
    private val xInA   = Use ("x")
    private val declAz = VarDecl ("z", Use ("int"))
    private val zInA   = Use ("z")

    val ast =
        Program (Block (
            List (declRx,
                  AssignStmt (xInR, zInR),
                  declRz,
                  AssignStmt (yInR, Use ("x")),
                  ClassDecl ("A", None, Block (
                      List (declAz,
                            AssignStmt (xInA, zInA),
                            AssignStmt (yInA, Use ("z"))))))))

    test ("bindings at the same nesting level are resolved") {
        expect (declRx) (xInR->decl)
    }

    test ("bindings at an outer nesting level are resolved") {
        expect (declRx) (xInA->decl)
    }

    test ("names can be declared after use") {
        expect (declRz) (zInR->decl)
    }

    test ("a missing declaration for a top-level use is detected") {
        assert (isUnknown (yInR->decl))
    }

    test ("a missing declaration for a nested use is detected") {
        assert (isUnknown (yInA->decl))
    }

    test ("a local shadowing binding is resolved") {
        expect (declAz) (zInA->decl)
    }

}

