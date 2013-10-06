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

    import org.kiama.attribution.Attribution.initTree
    import org.kiama.example.picojava.AbstractSyntax._
    import org.kiama.example.picojava.NameResolution._
    import org.kiama.example.picojava.TypeAnalysis._

    // For the actual program text, see DotNameResolutionTests.pj

    private val axInA   = Use ("x")
    private val declAAx = VarDecl (Use ("int"), "x")
    private val bxInBB  = Use ("x")
    private val byInBB  = Use ("y")
    private val BBinBB  = Use ("BB")
    private val declAA  = ClassDecl ("AA", None, Block (List (declAAx)))
    private val declBB  = ClassDecl ("BB", Some (Use ("AA")), Block (
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
    initTree (ast)

    test ("class members are resolved") {
        assertResult (declAAx) (axInA->decl)
    }

    test ("nested classes are resolved") {
        assertResult (declBB) (BBinBB->decl)
    }

    test ("nested names hide outer ones") {
        assertResult (declAAx) (bxInBB->decl)
    }

    test ("non-members in scope are not resolved as members") {
        assert (isUnknown (byInBB->decl))
    }

}
