/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
