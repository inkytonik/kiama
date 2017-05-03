/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.prolog

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Tests that check that the queries run correctly. I.e., given a base
 * Prolog file containing definitions, that running specific queries
 * over those definitions give the expected results.
 */
class InterpreterTests extends ParseTests {

    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
    import org.bitbucket.inkytonik.kiama.util.StringEmitter
    import org.scalatest.matchers.{Matcher, MatchResult}
    import scala.io.Source

    val parsers = new SyntaxAnalyser(positions)

    val path = "src/test/scala/org/bitbucket/inkytonik/kiama/example/prolog/tests"

    /**
     * Matcher for running a query against a file of definitions and comparing
     * against the expected result.
     */
    def queryTo[T](basename : String, expected : String) =
        new Matcher[String] {
            def apply(term : String) = {
                val filename = s"$path/$basename"
                parsers.program(Source.fromFile(filename).mkString) match {
                    case Success(program, _) =>
                        parsers.query(term) match {
                            case Success(query, _) =>
                                val interpreter = new Interpreter
                                val emitter = new StringEmitter
                                interpreter.interpret(query, program, emitter)
                                val value = emitter.result
                                MatchResult(
                                    value == expected,
                                    s""""$term" evaluated to "$value" not expected "$expected"""",
                                    s""""$term" evaluated to "$expected""""
                                )
                            case result : NoSuccess =>
                                MatchResult(false, s""""$term" ${result.toMessage}""", "NOT USED")
                        }
                    case result : NoSuccess =>
                        MatchResult(false, s""""$filename" ${result.toMessage}""", "NOT USED")
                }
            }
        }

    test("queries that produce no results work") {
        "likes." should queryTo("likes.pl", "")
        "unknown(mary,X)." should queryTo("likes.pl", "")
        "likes(mary,john)." should queryTo("likes.pl", "")
        "likes(X,boris)." should queryTo("likes.pl", "")
        "male(victoria)." should queryTo("family.pl", "")
    }

    test("simple 'yes' queries work") {
        "likes(mary,wine)." should queryTo("likes.pl", "yes\n")
        "male(bob)." should queryTo("family.pl", "yes\n")
        "female(victoria)." should queryTo("family.pl", "yes\n")
        "parent(alice,albert)." should queryTo("family.pl", "yes\n")
    }

    test("queries with variables work") {
        "likes(john,Y)." should queryTo(
            "likes.pl",
            """Y = wine
              |Y = mary
              |Y = mary
              |""".stripMargin
        )
        "likes(X,mary)." should queryTo(
            "likes.pl",
            """X = john
              |X = john
              |""".stripMargin
        )
        "likes(X,wine)." should queryTo(
            "likes.pl",
            """X = mary
              |X = john
              |""".stripMargin
        )
        "likes(X,Y)." should queryTo(
            "likes.pl",
            """X = mary Y = food
              |X = mary Y = wine
              |X = john Y = wine
              |X = john Y = mary
              |X = john Y = mary
              |""".stripMargin
        )
        "male(X)." should queryTo(
            "family.pl",
            """X = albert
              |X = edward
              |X = bob
              |""".stripMargin
        )
        "parent(alice,Y)." should queryTo(
            "family.pl",
            """Y = albert
              |Y = victoria
              |Y = bob
              |""".stripMargin
        )
        "father(edward,Y)." should queryTo(
            "family.pl",
            """Y = victoria
              |Y = albert
              |""".stripMargin
        )
        "father(X,Y)." should queryTo(
            "family.pl",
            """X = edward Y = victoria
              |X = edward Y = albert
              |""".stripMargin
        )
        "daughter(victoria,Y)." should queryTo(
            "family.pl",
            """Y = edward
              |Y = alice
              |""".stripMargin
        )
    }

}
