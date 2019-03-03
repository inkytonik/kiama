/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.bitbucket.inkytonik.kiama
package example.picojava.tests

import org.bitbucket.inkytonik.kiama.util.KiamaTests

class DotNameResolutionTests extends KiamaTests {

    import org.bitbucket.inkytonik.kiama.example.picojava.ErrorCheck
    import org.bitbucket.inkytonik.kiama.example.picojava.PicoJavaTree._

    // For the actual program text, see DotNameResolutionTests.pj

    val axInA = Use("x")
    val declAAx = VarDecl(Use("int"), "x")
    val bxInBB = Use("x")
    val byInBB = Use("y")
    val BBinBB = Use("BB")
    val declAA = ClassDecl("AA", None, Block(Vector(declAAx)))
    val declBB = ClassDecl("BB", Some(Use("AA")), Block(
        Vector(
            VarDecl(BBinBB, "b"),
            AssignStmt(
                Dot(Use("b"), byInBB),
                Dot(Use("b"), bxInBB)
            )
        )
    ))

    val ast =
        Program(Block(
            Vector(ClassDecl("A", None, Block(
                Vector(
                    VarDecl(Use("int"), "y"),
                    VarDecl(Use("AA"), "a"),
                    AssignStmt(Use("x"), Dot(Use("a"), axInA)),
                    declAA,
                    declBB
                )
            )))
        ))

    val tree = new PicoJavaTree(ast)
    val analyser = new ErrorCheck(tree)
    import analyser._

    test("class members are resolved") {
        decl(axInA) shouldBe declAAx
    }

    test("nested classes are resolved") {
        decl(BBinBB) shouldBe declBB
    }

    test("nested names hide outer ones") {
        decl(bxInBB) shouldBe declAAx
    }

    test("non-members in scope are not resolved as members") {
        isUnknown(decl(byInBB)) shouldBe true
    }

}
