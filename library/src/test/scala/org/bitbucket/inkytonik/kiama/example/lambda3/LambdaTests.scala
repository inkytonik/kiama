/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda3

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Simple lambda calculus query tests.
 */
class LambdaTests extends ParseTests {

    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
    import org.bitbucket.inkytonik.kiama.rewriting.NominalTree.Name
    import org.scalatest.matchers.{Matcher, MatchResult}

    /**
     * Abstract syntax constructs that are common to all nominal rewriters.
     * These definitions need to be separate from the NominalRewriter class
     * so that the classes here don't get an outer field referring to an
     * instance of that class.
     */
    object NominalTree

    val parsers = new SyntaxAnalyser(positions)

    /**
     * Matcher for query parsing and evaluation with expected value.
     */
    def queryTo[T](expected : T) =
        new Matcher[String] {
            def apply(term : String) =
                parsers.query(term) match {
                    case Success(query, _) =>
                        val evaluator = new Evaluator
                        val value = evaluator.execute(query)
                        MatchResult(
                            value == expected,
                            s""""$term" evaluated to "$value" not expected "$expected"""",
                            s""""$term" evaluated to "$expected""""
                        )
                    case result : NoSuccess =>
                        MatchResult(false, s""""$term" ${result.toMessage}"""", "NOT USED")
                }
        }

    /**
     * Same as `queryTo` but the expected value comes in string form and is
     * parsed as an expresion to obtain the actual expected value.
     */
    def evalTo[T](expectedStr : String) =
        new Matcher[String] {
            def apply(term : String) =
                parsers.exp(expectedStr) match {
                    case Success(exp, _) =>
                        queryTo(exp)(term)
                    case result : NoSuccess =>
                        MatchResult(false, s""""$expectedStr" ${result.toMessage}"""", "NOT USED")
                }
        }

    // Freshness

    test("freshness evaluates correctly") {
        "e # 1" should queryTo(true)
        "e # e" should queryTo(false)
        "e # f" should queryTo(true)
        "e # (\\e . e)" should queryTo(true)
        "e # (\\f . f)" should queryTo(true)
        "e # (\\f . e)" should queryTo(false)
        "e # (\\f . f) (\\g . g)" should queryTo(true)
        "e # (\\f . f) (\\e . e)" should queryTo(true)
        "e # (\\f . f) (\\g . e)" should queryTo(false)
    }

    // Swapping

    test("swapping evaluates correctly") {
        "(a <-> a) a" should evalTo("a")
        "(a <-> a) b" should evalTo("b")
        "(a <-> b) a" should evalTo("b")
        "(a <-> b) b" should evalTo("a")
        "(a <-> b) c" should evalTo("c")
        "(a <-> b) a b" should evalTo("b a")
        "(a <-> b) a c" should evalTo("b c")
        "(a <-> b) c b" should evalTo("c a")
        "(a <-> b) \\x . x" should evalTo("\\x . x")
        "(a <-> b) \\a . a" should evalTo("\\b . b")
        "(a <-> b) \\b . b" should evalTo("\\a . a")
        "(a <-> b) (\\b . b a) (\\x . x)" should evalTo("(\\a . a b) (\\x . x)")
        "(a <-> b) (\\a . b) (\\b . a)" should evalTo("(\\b . a) (\\a . b)")
    }

    // Alpha equivalence

    test("alpha equivalence evaluates correctly") {
        "a === a" should queryTo(true)
        "a === b" should queryTo(false)
        "a === \\x . x" should queryTo(false)
        "a === b c" should queryTo(false)
        "\\a . a === \\b . b" should queryTo(true)
        "\\a . a === \\b . a" should queryTo(false)
        "\\a . a === a b" should queryTo(false)
        "\\a . \\a . a === \\b . \\a . b" should queryTo(false)
        "\\a . \\a . a === \\a . \\a . b" should queryTo(false)
        "\\a . \\a . a === \\b . \\a . a" should queryTo(true)
        "(\\x . x) (\\y. y) === (\\a . a) (\\b . b)" should queryTo(true)
        "(\\x . x) (\\y. y) === (\\x . x) (\\b . b)" should queryTo(true)
        "(\\x . x) (\\y. y) === (\\x . y) (\\y . x)" should queryTo(false)
    }

    // Substitution

    test("substitution evaluates correctly") {
        "[a -> 1] a" should evalTo("1")
        "[a -> 1] b" should evalTo("b")
        "[a -> 1] \\x . x" should evalTo("\\x0 . x0")
        "[a -> 1] \\x . a" should evalTo("\\x0 . 1")
        "[a -> b] \\a . a" should evalTo("\\a0 . a0")
        "[a -> b] \\b . a" should evalTo("\\b0 . b")
        "[a -> b] \\b . b a" should evalTo("\\b0 . b0 b")
        "[a -> b] \\b . \\a . a" should evalTo("\\b0 . \\a1 . a1")
    }

    // Free variables

    test("free variables are calculated correctly") {
        "fv 1" should queryTo(Set())
        "fv a" should queryTo(Set(Name("a")))
        "fv a b" should queryTo(Set(Name("a"), Name("b")))
        "fv \\a . a" should queryTo(Set())
        "fv \\a . b" should queryTo(Set(Name("b")))
        "fv \\a . a b" should queryTo(Set(Name("b")))
        "fv \\a . b a" should queryTo(Set(Name("b")))
        "fv \\a . b c" should queryTo(Set(Name("b"), Name("c")))
        "fv \\a . \\b . a" should queryTo(Set())
        "fv \\a . \\b . b" should queryTo(Set())
        "fv \\a . \\b . c" should queryTo(Set(Name("c")))
    }

    // Evaluation

    test("basic evaluation works correctly") {
        "1" should evalTo("1")
        "a" should evalTo("a")
        "a b" should evalTo("a b")
        "a (\\b . b)" should evalTo("a (\\b . b)")
        "a (\\b . b) 1" should evalTo("(a (\\b . b)) 1")
        "a ((\\b . b) 1)" should evalTo("a ((\\b . b) 1)")
        "(\\a . a) 1" should evalTo("1")
        "(\\a . a) a" should evalTo("a")
        "(\\a . a) b" should evalTo("b")
        "(\\a . a) (\\b . b) a" should evalTo("a")
        "(\\a . a c) (\\b . b)" should evalTo("c")
        "(\\a . \\b . a b) b" should evalTo("\\b0 . b b0")
    }

}
