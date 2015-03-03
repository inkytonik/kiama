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

class DotNameResolutionTests extends Tests {

    import org.kiama.example.picojava.ErrorCheck
    import org.kiama.example.picojava.PicoJavaTree._

    // For the actual program text, see DotNameResolutionTests.pj

    val axInA   = Use ("x")
    val declAAx = VarDecl (Use ("int"), "x")
    val bxInBB  = Use ("x")
    val byInBB  = Use ("y")
    val BBinBB  = Use ("BB")
    val declAA  = ClassDecl ("AA", None, Block (List (declAAx)))
    val declBB  = ClassDecl ("BB", Some (Use ("AA")), Block (
                      List (VarDecl (BBinBB, "b"),
                            AssignStmt (Dot (Use ("b"), byInBB),
                                        Dot (Use ("b"), bxInBB)))))

    val ast =
        Program (Block (
            List (ClassDecl ("A", None, Block (
                List (VarDecl (Use ("int"), "y"),
                      VarDecl (Use ("AA"), "a"),
                      AssignStmt (Use ("x"), Dot (Use ("a"), axInA)),
                      declAA,
                      declBB))))))

    val tree = new PicoJavaTree (ast)
    val analyser = new ErrorCheck (tree)
    import analyser._

    test ("class members are resolved") {
        assertResult (declAAx) (decl (axInA))
    }

    test ("nested classes are resolved") {
        assertResult (declBB) (decl (BBinBB))
    }

    test ("nested names hide outer ones") {
        assertResult (declAAx) (decl (bxInBB))
    }

    test ("non-members in scope are not resolved as members") {
        assert (isUnknown (decl (byInBB)))
    }

}
