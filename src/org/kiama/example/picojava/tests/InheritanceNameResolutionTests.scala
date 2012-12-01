/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2012 Anthony M Sloane, Macquarie University.
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
import org.scalatest.junit.JUnitRunner

class InheritanceNameResolutionTests extends Tests {

    import org.kiama.attribution.Attribution.initTree
    import org.kiama.example.picojava.AbstractSyntax._
    import org.kiama.example.picojava.NameResolution._

    // For the actual program text, see InheritanceNameResolutionTests.pj

    private val declAa  = VarDecl (Use ("int"), "a")
    private val aInAA   = Use ("a")
    private val declAAb = VarDecl (Use ("int"), "b")
    private val bInAA   = Use ("b")
    private val AinB    = Use ("A")
    private val aInB    = Use ("a")
    private val declBc  = VarDecl (Use ("int"), "c")
    private val cInB    = Use ("c")
    private val AAinBB  = Use ("AA")
    private val aInBB   = Use ("a")
    private val declAAe = VarDecl (Use ("int"), "e")
    private val eInBB   = Use ("e")
    private val fInBB   = Use ("f")
    private val declBf  = VarDecl (Use ("int"), "f")

    private val declAA = ClassDecl ("AA", None, Block(
                             List (declAAb,
                                   VarDecl (Use ("int"), "d"),
                                   declAAe,
                                   AssignStmt (aInAA, bInAA))))

    private val declA = ClassDecl ("A", None, Block(
                            List (declAa,
                                  VarDecl (Use ("int"), "b"),
                                  VarDecl (Use ("int"), "c"),
                                  declAA)))

    val ast =
        Program (Block (
            List (declA,
                  ClassDecl ("B", Some (AinB), Block (
                      List (declBc,
                            VarDecl (Use ("int"), "e"),
                            declBf,
                            AssignStmt (aInB, cInB),
                            ClassDecl ("BB", Some (AAinBB), Block (
                                List (VarDecl (Use ("int"), "d"),
                                      AssignStmt (aInBB, Use ("d")),
                                      AssignStmt (eInBB, fInBB))))))))))
    initTree (ast)

    test ("members are resolved in nested classes") {
        expectResult (declAa) (aInAA->decl)
    }

    test ("nested members shadow outer members") {
        expectResult (declAAb) (bInAA->decl)
    }

    test ("class names are resolved in extends clauses") {
        expectResult (declA) (AinB->decl)
    }

    test ("inherited members are resolved") {
        expectResult (declAa) (aInB->decl)
    }

    test ("local members hide inherited ones") {
        expectResult (declBc) (cInB->decl)
    }

    test ("inherited inner classes are resolved") {
        expectResult (declAA) (AAinBB->decl)
    }

    test ("inner references to members of outer class are resolved") {
        expectResult (declBf) (fInBB->decl)
    }

    test ("inner references to inherited members of outer class are resolved") {
        expectResult (declAa) (aInBB->decl)
    }

    test ("inherited members shadow outer occurrences of the same name") {
        expectResult (declAAe) (eInBB->decl)
    }

}
