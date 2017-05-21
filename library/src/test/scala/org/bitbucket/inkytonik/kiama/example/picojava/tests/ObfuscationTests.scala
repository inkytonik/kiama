/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2013-2017 Matthew Roberts, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.picojava.tests

import org.bitbucket.inkytonik.kiama.util.PrettyPrinterTests

class ObfuscationTests extends PrettyPrinterTests {

    import org.bitbucket.inkytonik.kiama.example.picojava.{ErrorCheck, Obfuscator}
    import org.bitbucket.inkytonik.kiama.example.picojava.PicoJavaTree._
    import org.bitbucket.inkytonik.kiama.example.picojava.PrettyPrinter.format

    // For the actual program text, see ObfuscationTest.pj

    // The tree to obfuscate

    val ast =
        Program(
            Block(
                Vector(
                    ClassDecl(
                        "ALongClassName",
                        None,
                        Block(
                            Vector(
                                VarDecl(Use("int"), "avar"),
                                VarDecl(Use("int"), "bvar"),
                                ClassDecl(
                                    "NestedClass",
                                    None,
                                    Block(
                                        Vector(
                                            VarDecl(Use("int"), "item"),
                                            AssignStmt(
                                                Use("avar"),
                                                Use("item")
                                            )
                                        )
                                    )
                                ),
                                VarDecl(Use("NestedClass"), "object"),
                                AssignStmt(
                                    Dot(Use("object"), Use("item")),
                                    Use("bvar")
                                )
                            )
                        )
                    ),
                    ClassDecl(
                        "AnotherClassName",
                        None,
                        Block(
                            Vector(
                                VarDecl(Use("int"), "avar"),
                                VarDecl(Use("ALongClassName"), "object"),
                                AssignStmt(
                                    Use("avar"),
                                    Dot(Use("object"), Use("bvar"))
                                )
                            )
                        )
                    )
                )
            )
        )

    // The expected obfuscated tree

    val expobast =
        Program(
            Block(
                Vector(
                    ClassDecl(
                        "n0",
                        None,
                        Block(
                            Vector(
                                VarDecl(Use("int"), "n1"),
                                VarDecl(Use("int"), "n2"),
                                ClassDecl(
                                    "n3",
                                    None,
                                    Block(
                                        Vector(
                                            VarDecl(Use("int"), "n4"),
                                            AssignStmt(Use("n7"), Use("n4"))
                                        )
                                    )
                                ),
                                VarDecl(Use("n3"), "n5"),
                                AssignStmt(
                                    Dot(Use("n5"), Use("n4")),
                                    Use("n2")
                                )
                            )
                        )
                    ),
                    ClassDecl(
                        "n6",
                        None,
                        Block(
                            Vector(
                                VarDecl(Use("int"), "n7"),
                                VarDecl(Use("n0"), "n8"),
                                AssignStmt(
                                    Use("n7"),
                                    Dot(Use("n8"), Use("n2"))
                                )
                            )
                        )
                    )
                )
            )
        )

    val tree = new PicoJavaTree(ast)
    val analysis = new ErrorCheck(tree)

    val obfuscator = new Obfuscator(analysis)

    val obast = obfuscator.obfuscate(ast)

    test("obfuscation produces correct program (pretty printed)") {
        format(obast).layout shouldBe format(expobast).layout
    }

    test("obfuscation produces correct program") {
        obast shouldBe expobast
    }

}
