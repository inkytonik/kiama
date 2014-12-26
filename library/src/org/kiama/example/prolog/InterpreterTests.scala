/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package example.prolog

import org.kiama.util.RegexParserTests

/**
 * Tests that check that the queries run correctly. I.e., given a base
 * Prolog file containing definitions, that running specific queries
 * over those definitions give the expected results.
 */
class InterpreterTests extends SyntaxAnalyser with RegexParserTests {

    import org.kiama.util.StringEmitter
    import scala.io.Source

    /**
     * Create an interpreter test. The file name `fn` is the one that should be
     * loaded to obtain definitions. It is assumed to be relative to the Prolog
     * example test directory. The query string `q` is run against the loaded
     * definitions to obtain some textual output, which is compared against the
     * expected output.
     */
    def querytest (fn : String, q : String, exp : String) {
        val filename = s"src/org/kiama/example/prolog/tests/$fn"
        test (s"$q on $filename") {
            assertParseCheck (Source.fromFile (filename).mkString, program) {
                programtree =>
                    assertParseCheck (q, query) {
                        querytree =>
                            val interpreter = new Interpreter
                            val emitter = new StringEmitter
                            interpreter.interpret (querytree, programtree, emitter)
                            assertResult (exp) (emitter.result)
                    }
            }
        }
    }

    querytest ("likes.pl", "likes.", "")

    querytest ("likes.pl", "unknown(mary,X).", "")

    querytest ("likes.pl", "likes(mary,wine).", "yes\n")

    querytest ("likes.pl", "likes(mary,john).", "")

    querytest ("likes.pl", "likes(john,Y).",
               """Y = wine
                 |Y = mary
                 |Y = mary
                 |""".stripMargin)

    querytest ("likes.pl", "likes(X,boris).", "")

    querytest ("likes.pl", "likes(X,mary).",
               """X = john
                 |X = john
                 |""".stripMargin)

    querytest ("likes.pl", "likes(X,wine).",
               """X = mary
                 |X = john
                 |""".stripMargin)

    querytest ("likes.pl", "likes(X,Y).",
               """X = mary Y = food
                 |X = mary Y = wine
                 |X = john Y = wine
                 |X = john Y = mary
                 |X = john Y = mary
                 |""".stripMargin)

    querytest ("family.pl", "male(bob).", "yes\n")

    querytest ("family.pl", "male(victoria).", "")

    querytest ("family.pl", "female(victoria).", "yes\n")

    querytest ("family.pl", "male(X).",
               """X = albert
                 |X = edward
                 |X = bob
                 |""".stripMargin)

    querytest ("family.pl", "parent(alice,albert).", "yes\n")

    querytest ("family.pl", "parent(alice,Y).",
               """Y = albert
                 |Y = victoria
                 |Y = bob
                 |""".stripMargin)

    querytest ("family.pl", "father(edward,Y).",
               """Y = victoria
                 |Y = albert
                 |""".stripMargin)

    querytest ("family.pl", "father(X,Y).",
               """X = edward Y = victoria
                 |X = edward Y = albert
                 |""".stripMargin)

    querytest ("family.pl", "daughter(victoria,Y).",
               """Y = edward
                 |Y = alice
                 |""".stripMargin)

}

