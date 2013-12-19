/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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
package example.lambda3

import org.kiama.util.RegexParserTests

/**
 * Simple lambda calculus query tests.
 */
class LambdaTests extends RegexParserTests with Parser {

    import LambdaTree._

    /**
     * Try to parse and evaluate str as a query then evaluate the query
     * and compare the outcome to `expected`. `T` is the result type of
     * the query.
     */
    def assertQuery[T] (str : String, expected : T) {
        assertParseCheck (str, query) {
            exp => {
                val evaluator = new Evaluator
                assertResult (expected) (evaluator.execute (exp))
            }
        }
    }

    /**
     * As for assertQuery except that the expected result is the `toString`
     * of the query result.
     */
    def assertQueryPrint[T] (str : String, expected : String) {
        assertParseCheck (str, query) {
            exp => {
                val evaluator = new Evaluator
                assertResult (expected) (evaluator.execute (exp).toString)
            }
        }
    }

    /**
     * As for assertQuery except that the expected result is a string
     * which is first parsed as an expression to obtain the expected result
     * value.
     */
    def assertQueryParse (str : String, expectedStr : String) {
        assertParseCheck (expectedStr, exp) {
            expected =>
                assertQuery (str, expected)
        }
    }

    // Test construction short-hands

    def mkvaluetest[T] (s : String, r : T) {
        test (s"$s is $r") {
            assertQuery (s, r)
        }
    }

    def mkprinttest (s : String, r : String) {
        test (s"$s is $r") {
            assertQueryPrint (s, r)
        }
    }

    def mkparsetest (s : String, r : String) {
        test (s"$s is $r") {
            assertQueryParse (s, r)
        }
    }

    // Freshness

    mkvaluetest ("e # 1", true)
    mkvaluetest ("e # e", false)
    mkvaluetest ("e # f", true)
    mkvaluetest ("e # (\\e . e)", true)
    mkvaluetest ("e # (\\f . f)", true)
    mkvaluetest ("e # (\\f . e)", false)
    mkvaluetest ("e # (\\f . f) (\\g . g)", true)
    mkvaluetest ("e # (\\f . f) (\\e . e)", true)
    mkvaluetest ("e # (\\f . f) (\\g . e)", false)

    // Swapping

    mkparsetest ("(a <-> a) a", "a")
    mkparsetest ("(a <-> a) b", "b")
    mkparsetest ("(a <-> b) a", "b")
    mkparsetest ("(a <-> b) b", "a")
    mkparsetest ("(a <-> b) c", "c")
    mkparsetest ("(a <-> b) a b", "b a")
    mkparsetest ("(a <-> b) a c", "b c")
    mkparsetest ("(a <-> b) c b", "c a")
    mkparsetest ("(a <-> b) \\x . x", "\\x . x")
    mkparsetest ("(a <-> b) \\a . a", "\\b . b")
    mkparsetest ("(a <-> b) \\b . b", "\\a . a")
    mkparsetest ("(a <-> b) (\\b . b a) (\\x . x)", "(\\a . a b) (\\x . x)")
    mkparsetest ("(a <-> b) (\\a . b) (\\b . a)", "(\\b . a) (\\a . b)")

    // Alpha equivalence

    mkvaluetest ("a === a", true)
    mkvaluetest ("a === b", false)
    mkvaluetest ("a === \\x . x", false)
    mkvaluetest ("a === b c", false)
    mkvaluetest ("\\a . a === \\b . b", true)
    mkvaluetest ("\\a . a === \\b . a", false)
    mkvaluetest ("\\a . a === a b", false)
    mkvaluetest ("\\a . \\a . a === \\b . \\a . b", false)
    mkvaluetest ("\\a . \\a . a === \\a . \\a . b", false)
    mkvaluetest ("\\a . \\a . a === \\b . \\a . a", true)
    mkvaluetest ("(\\x . x) (\\y. y) === (\\a . a) (\\b . b)", true)
    mkvaluetest ("(\\x . x) (\\y. y) === (\\x . x) (\\b . b)", true)
    mkvaluetest ("(\\x . x) (\\y. y) === (\\x . y) (\\y . x)", false)

    // Substitution

    mkparsetest ("[a -> 1] a", "1")
    mkparsetest ("[a -> 1] b", "b")
    mkparsetest ("[a -> 1] \\x . x", "\\x0 . x0")
    mkparsetest ("[a -> 1] \\x . a", "\\x0 . 1")
    mkparsetest ("[a -> b] \\a . a", "\\a0 . a0")
    mkparsetest ("[a -> b] \\b . a", "\\b0 . b")
    mkparsetest ("[a -> b] \\b . b a", "\\b0 . b0 b")
    mkparsetest ("[a -> b] \\b . \\a . a", "\\b0 . \\a1 . a1")

    // Free variables

    mkprinttest ("fv 1", "Set()")
    mkprinttest ("fv a", "Set(a)")
    mkprinttest ("fv a b", "Set(a, b)")
    mkprinttest ("fv \\a . a", "Set()")
    mkprinttest ("fv \\a . b", "Set(b)")
    mkprinttest ("fv \\a . a b", "Set(b)")
    mkprinttest ("fv \\a . b a", "Set(b)")
    mkprinttest ("fv \\a . b c", "Set(b, c)")
    mkprinttest ("fv \\a . \\b . a", "Set()")
    mkprinttest ("fv \\a . \\b . b", "Set()")
    mkprinttest ("fv \\a . \\b . c", "Set(c)")

    // Evaluation

    mkparsetest ("1", "1")
    mkparsetest ("a", "a")
    mkparsetest ("a b", "a b")
    mkparsetest ("a (\\b . b)", "a (\\b . b)")
    mkparsetest ("a (\\b . b) 1", "(a (\\b . b)) 1")
    mkparsetest ("a ((\\b . b) 1)", "a ((\\b . b) 1)")
    mkparsetest ("(\\a . a) 1", "1")
    mkparsetest ("(\\a . a) a", "a")
    mkparsetest ("(\\a . a) b", "b")
    mkparsetest ("(\\a . a) (\\b . b) a", "a")
    mkparsetest ("(\\a . a c) (\\b . b)", "c")
    mkparsetest ("(\\a . \\b . a b) b", "\\b0 . b b0")

}
