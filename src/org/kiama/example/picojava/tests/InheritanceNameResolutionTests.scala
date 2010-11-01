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
class InheritanceNameResolutionTests extends FunSuite {

    import org.kiama.example.picojava.AbstractSyntax._
    import org.kiama.example.picojava.NameResolution._
    import org.kiama.example.picojava.TypeAnalysis._

    // For the actual program text, see InheritanceNameResolutionTests.pj

    private val declAa  = VarDecl ("a", Use ("int"))
    private val aInAA   = Use ("a")
    private val declAAb = VarDecl ("b", Use ("int"))
    private val bInAA   = Use ("b")
    private val AinB    = Use ("A")
    private val aInB    = Use ("a")
    private val declBc  = VarDecl ("c", Use ("int"))
    private val cInB    = Use ("c")
    private val AAinBB  = Use ("AA")
    private val aInBB   = Use ("a")
    private val declAAe = VarDecl ("e", Use ("int"))
    private val eInBB   = Use ("e")
    private val fInBB   = Use ("f")
    private val declBf  = VarDecl ("f", Use ("int"))

    private val declAA = ClassDecl ("AA", None, Block(
                             List (declAAb,
                                   VarDecl ("d", Use ("int")),
                                   declAAe,
                                   AssignStmt (aInAA, bInAA))))

    private val declA = ClassDecl ("A", None, Block(
                            List (declAa,
                                  VarDecl ("b", Use ("int")),
                                  VarDecl ("c", Use ("int")),
                                  declAA)))

    val ast =
        Program (Block (
            List (declA,
                  ClassDecl ("B", Some (AinB), Block (
                      List (declBc,
                            VarDecl ("e", Use ("int")),
                            declBf,
                            AssignStmt (aInB, cInB),
                            ClassDecl ("BB", Some (AAinBB), Block (
                                List (VarDecl ("d", Use ("int")),
                                      AssignStmt (aInBB, Use ("d")),
                                      AssignStmt (eInBB, fInBB))))))))))

    test ("members are resolved in nested classes") {
        expect (declAa) (aInAA->decl)
    }

    test ("nested members shadow outer members") {
        expect (declAAb) (bInAA->decl)
    }

    test ("class names are resolved in extends clauses") {
        expect (declA) (AinB->decl)
    }

    test ("inherited members are resolved") {
        expect (declAa) (aInB->decl)
    }

    test ("local members hide inherited ones") {
        expect (declBc) (cInB->decl)
    }

    test ("inherited inner classes are resolved") {
        expect (declAA) (AAinBB->decl)
    }

    test ("inner references to members of outer class are resolved") {
        expect (declBf) (fInBB->decl)
    }

    test ("inner references to inherited members of outer class are resolved") {
        expect (declAa) (aInBB->decl)
    }

    test ("inherited members shadow outer occurrences of the same name") {
        expect (declAAe) (eInBB->decl)
    }

}
