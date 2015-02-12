/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2015 Anthony M Sloane, Macquarie University.
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

/**
 * Test of many picojava features together, Due to Niklas Fors.
 */
class CombinedTests extends Tests {

    import org.kiama.example.picojava.ErrorCheck
    import org.kiama.example.picojava.PicoJavaTree._
    import scala.collection.immutable.Seq

    // For the actual program text, see CombinedTests.pj

    val ast =
        Program (
            Block (
                List (
                    ClassDecl (
                        "A",
                        None,
                        Block (
                            List (
                                VarDecl (Use ("boolean"), "a"),
                                AssignStmt (Use ("a"), BooleanLiteral ("true")),
                                ClassDecl (
                                    "AA",
                                    None,
                                    Block (
                                        List (VarDecl (Use ("boolean"), "aa"))))))),
                    ClassDecl (
                        "B",
                        Some (Use ("A")),
                        Block (
                            List (
                                VarDecl (Use ("boolean"), "b"),
                                AssignStmt (Use ("b"), Use ("a")),
                                VarDecl (Use ("A"), "refA"),
                                VarDecl (Use ("B"), "refB"),
                                AssignStmt (Use ("refA"), Use ("refB")),
                                AssignStmt (
                                    Dot (Use ("refB"), Use ("b")),
                                    Dot (Use ("refA"), Use ("a"))),
                                ClassDecl (
                                    "BB",
                                    Some (Use ("AA")),
                                    Block (
                                        List (
                                            VarDecl (Use ("boolean"), "bb"),
                                            AssignStmt (Use ("bb"), Use ("aa")),
                                            WhileStmt (
                                                Use ("b"),
                                                AssignStmt (Use ("b"), Use ("a"))))))))))))

    val tree = new PicoJavaTree (ast)
    val analyser = new ErrorCheck (tree)
    import analyser._

    test ("combined test program has no errors") {
        assertResult (0) (errors.size ())
    }

}
