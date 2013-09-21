/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2013 Matthew Roberts, Macquarie University.
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

package org.kiama
package example.picojava.tests

import org.kiama.util.Tests

class ObfuscationTests extends Tests {

    import org.kiama.attribution.Attribution.initTree
    import org.kiama.example.picojava.AbstractSyntax._
    import org.kiama.example.picojava.Obfuscate.obfuscate
    import org.kiama.example.picojava.PrettyPrinter.pretty

    // For the actual program text, see ObfuscationTest.pj

    // The tree to obfuscate

    val ast =
        Program (
            Block (
                List (
                    ClassDecl (
                        "ALongClassName",
                        None,
                        Block (
                            List (
                                VarDecl (Use ("int"), "avar"),
                                VarDecl (Use ("int"), "bvar"),
                                ClassDecl (
                                    "NestedClass",
                                    None,
                                    Block (
                                        List (
                                            VarDecl (Use ("int"), "item"),
                                            AssignStmt (
                                                Use ("avar"),
                                                Use ("item"))))),
                                VarDecl (Use ("NestedClass"), "object"),
                                AssignStmt (
                                    Dot (Use ("object"), Use ("item")),
                                    Use ("bvar"))))),
                    ClassDecl (
                        "AnotherClassName",
                        None,
                        Block (
                            List (
                                VarDecl (Use ("int"), "avar"),
                                VarDecl (Use ("ALongClassName"), "object"),
                                AssignStmt (
                                    Use ("avar"),
                                    Dot (Use ("object"), Use ("bvar")))))))))

    // The expected obfuscated tree

    val expobast =
        Program (
            Block (
                List (
                    ClassDecl (
                        "n0",
                        None,
                        Block (
                            List (
                                VarDecl (Use ("int"), "n1"),
                                VarDecl (Use ("int"), "n2"),
                                ClassDecl (
                                    "n3",
                                    None,
                                    Block (
                                        List (
                                            VarDecl (Use ("int"), "n4"),
                                            AssignStmt (Use ("n7"), Use ("n4"))))),
                                VarDecl (Use ("n3"), "n5"),
                                AssignStmt (
                                    Dot (Use ("n5"), Use ("n4")),
                                    Use ("n2"))))),
                    ClassDecl (
                        "n6",
                        None,
                        Block (
                            List (
                                VarDecl (Use ("int"), "n7"),
                                VarDecl (Use ("n0"), "n8"),
                                AssignStmt (
                                    Use ("n7"),
                                    Dot (Use ("n8"), Use ("n2")))))))))

    initTree (ast)

    // The obfuscated tree
    val obast = obfuscate (ast)

    test ("obfuscation produces correct program (pretty printed)") {
        assertResult (pretty (expobast)) (pretty (obast))
    }

    test ("obfuscation produces correct program") {
        assertResult (expobast) (obast)
    }

}

