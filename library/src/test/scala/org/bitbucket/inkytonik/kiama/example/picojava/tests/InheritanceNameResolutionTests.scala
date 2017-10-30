/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2017 Anthony M Sloane, Macquarie University.
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

class InheritanceNameResolutionTests extends KiamaTests {

    import org.bitbucket.inkytonik.kiama.example.picojava.ErrorCheck
    import org.bitbucket.inkytonik.kiama.example.picojava.PicoJavaTree._

    // For the actual program text, see InheritanceNameResolutionTests.pj

    val declAa = VarDecl(Use("int"), "a")
    val aInAA = Use("a")
    val declAAb = VarDecl(Use("int"), "b")
    val bInAA = Use("b")
    val AinB = Use("A")
    val aInB = Use("a")
    val declBc = VarDecl(Use("int"), "c")
    val cInB = Use("c")
    val AAinBB = Use("AA")
    val aInBB = Use("a")
    val declAAe = VarDecl(Use("int"), "e")
    val eInBB = Use("e")
    val fInBB = Use("f")
    val declBf = VarDecl(Use("int"), "f")

    val declAA = ClassDecl("AA", None, Block(
        Vector(
            declAAb,
            VarDecl(Use("int"), "d"),
            declAAe,
            AssignStmt(aInAA, bInAA)
        )
    ))

    val declA = ClassDecl("A", None, Block(
        Vector(
            declAa,
            VarDecl(Use("int"), "b"),
            VarDecl(Use("int"), "c"),
            declAA
        )
    ))

    val ast =
        Program(Block(
            Vector(
                declA,
                ClassDecl("B", Some(AinB), Block(
                    Vector(
                        declBc,
                        VarDecl(Use("int"), "e"),
                        declBf,
                        AssignStmt(aInB, cInB),
                        ClassDecl("BB", Some(AAinBB), Block(
                            Vector(
                                VarDecl(Use("int"), "d"),
                                AssignStmt(aInBB, Use("d")),
                                AssignStmt(eInBB, fInBB)
                            )
                        ))
                    )
                ))
            )
        ))

    val tree = new PicoJavaTree(ast)
    val analyser = new ErrorCheck(tree)
    import analyser._

    test("members are resolved in nested classes") {
        decl(aInAA) shouldBe declAa
    }

    test("nested members shadow outer members") {
        decl(bInAA) shouldBe declAAb
    }

    test("class names are resolved in extends clauses") {
        decl(AinB) shouldBe declA
    }

    test("inherited members are resolved") {
        decl(aInB) shouldBe declAa
    }

    test("local members hide inherited ones") {
        decl(cInB) shouldBe declBc
    }

    test("inherited inner classes are resolved") {
        decl(AAinBB) shouldBe declAA
    }

    test("inner references to members of outer class are resolved") {
        decl(fInBB) shouldBe declBf
    }

    test("inner references to inherited members of outer class are resolved") {
        decl(aInBB) shouldBe declAa
    }

    test("inherited members shadow outer occurrences of the same name") {
        decl(eInBB) shouldBe declAAe
    }

}
