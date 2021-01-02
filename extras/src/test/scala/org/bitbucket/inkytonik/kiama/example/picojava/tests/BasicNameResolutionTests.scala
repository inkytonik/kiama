/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
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

class BasicNameResolutionTests extends KiamaTests {

    import org.bitbucket.inkytonik.kiama.example.picojava.ErrorCheck
    import org.bitbucket.inkytonik.kiama.example.picojava.PicoJavaTree._

    // For the actual program text, see BasicNameResolutionTests.pj

    val declRx = VarDecl(Use("int"), "x")
    val xInR = Use("x")
    val declRz = VarDecl(Use("int"), "z")
    val zInR = Use("z")
    val yInR = Use("y")
    val yInA = Use("y")
    val xInA = Use("x")
    val declAz = VarDecl(Use("int"), "z")
    val zInA = Use("z")

    val ast =
        Program(Block(
            Vector(
                declRx,
                AssignStmt(xInR, zInR),
                declRz,
                AssignStmt(yInR, Use("x")),
                ClassDecl("A", None, Block(
                    Vector(
                        declAz,
                        AssignStmt(xInA, zInA),
                        AssignStmt(yInA, Use("z"))
                    )
                ))
            )
        ))

    val tree = new PicoJavaTree(ast)
    val analyser = new ErrorCheck(tree)
    import analyser._

    test("bindings at the same nesting level are resolved") {
        decl(xInR) shouldBe declRx
    }

    test("bindings at an outer nesting level are resolved") {
        decl(xInA) shouldBe declRx
    }

    test("names can be declared after use") {
        decl(zInR) shouldBe declRz
    }

    test("a missing declaration for a top-level use is detected") {
        isUnknown(decl(yInR)) shouldBe true
    }

    test("a missing declaration for a nested use is detected") {
        isUnknown(decl(yInA)) shouldBe true
    }

    test("a local shadowing binding is resolved") {
        decl(zInA) shouldBe declAz
    }

}
