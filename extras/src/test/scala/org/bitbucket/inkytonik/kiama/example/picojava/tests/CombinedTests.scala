/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2021 Anthony M Sloane, Macquarie University.
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

/**
 * Test of many picojava features together, Due to Niklas Fors.
 */
class CombinedTests extends KiamaTests {

    import org.bitbucket.inkytonik.kiama.example.picojava.ErrorCheck
    import org.bitbucket.inkytonik.kiama.example.picojava.PicoJavaTree._

    // For the actual program text, see CombinedTests.pj

    val ast =
        Program(
            Block(
                Vector(
                    ClassDecl(
                        "A",
                        None,
                        Block(
                            Vector(
                                VarDecl(Use("boolean"), "a"),
                                AssignStmt(Use("a"), BooleanLiteral("true")),
                                ClassDecl(
                                    "AA",
                                    None,
                                    Block(
                                        Vector(VarDecl(Use("boolean"), "aa"))
                                    )
                                )
                            )
                        )
                    ),
                    ClassDecl(
                        "B",
                        Some(Use("A")),
                        Block(
                            Vector(
                                VarDecl(Use("boolean"), "b"),
                                AssignStmt(Use("b"), Use("a")),
                                VarDecl(Use("A"), "refA"),
                                VarDecl(Use("B"), "refB"),
                                AssignStmt(Use("refA"), Use("refB")),
                                AssignStmt(
                                    Dot(Use("refB"), Use("b")),
                                    Dot(Use("refA"), Use("a"))
                                ),
                                ClassDecl(
                                    "BB",
                                    Some(Use("AA")),
                                    Block(
                                        Vector(
                                            VarDecl(Use("boolean"), "bb"),
                                            AssignStmt(Use("bb"), Use("aa")),
                                            WhileStmt(
                                                Use("b"),
                                                AssignStmt(Use("b"), Use("a"))
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

    val tree = new PicoJavaTree(ast)
    val analyser = new ErrorCheck(tree)
    import analyser._

    test("combined test program has no errors") {
        errors.size shouldBe 0
    }

}
