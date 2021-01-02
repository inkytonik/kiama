/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

import org.bitbucket.inkytonik.kiama.util.ParseTests

/**
 * Lambda calculus tests.
 */
class LambdaTests extends ParseTests {

    import Evaluators.{evaluatorFor, mechanisms}
    import LambdaTree._
    import PrettyPrinter.formattedLayout
    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{all => rwall, _}
    import org.bitbucket.inkytonik.kiama.rewriting.Strategy
    import org.bitbucket.inkytonik.kiama.util.{Messaging, Positions, StringSource}
    import org.scalatest.matchers.{Matcher, MatchResult}

    val positions = new Positions
    val messaging = new Messaging(positions)
    val parsers = new SyntaxAnalyser(positions)

    /**
     * Parse and analyse the term string and return a collection of the
     * messages that result. It actually runs both `errors` and `errors2`,
     * returning a single set of messages if they are the same, otherwise
     * a composite set is returned.
     */
    def analyse(term : String) : String = {
        parsers.parseAll(parsers.exp, StringSource(term)) match {
            case Success(exp, _) =>
                val tree = new LambdaTree(exp)
                val analyser = new Analyser(tree)
                val errors = messaging.formatMessages(analyser.errors)
                val errors2 = messaging.formatMessages(analyser.errors2)
                if (errors == errors2)
                    errors
                else
                    "There were differences between 'errors' and 'errors2'\n" +
                        "errors:\n" + errors + "errors2:\n" + errors2
            case n : NoSuccess =>
                n.message
        }
    }

    test("an unknown variable by itself is reported") {
        analyse("y") shouldBe
            """1:1:error: 'y' unknown
              |y
              |^
              |""".stripMargin
    }

    test("an unknown variable in an abstraction is reported (typed)") {
        analyse("""\x : Int . x + y""") shouldBe
            """1:16:error: 'y' unknown
              |\x : Int . x + y
              |               ^
              |""".stripMargin
    }

    test("an unknown variable in an abstraction is reported (untyped)") {
        analyse("""\x . x + y""") shouldBe
            """1:10:error: 'y' unknown
              |\x . x + y
              |         ^
              |""".stripMargin
    }

    test("an Int -> Int cannot be used as an Int (typed)") {
        analyse("""(\x : Int -> Int . x + 1) (\y : Int . y)""") shouldBe
            """1:20:error: expected Int, found Int -> Int
              |(\x : Int -> Int . x + 1) (\y : Int . y)
              |                   ^
              |""".stripMargin
    }

    test("an Int -> Int can be used as an Int (untyped)") {
        analyse("""(\x . x + 1) (\y . y)""") shouldBe empty
    }

    test("an Int cannot be passed to an Int -> Int (typed)") {
        analyse("""(\x : Int -> Int . x 4) 3""") shouldBe
            """1:25:error: expected Int -> Int, found Int
              |(\x : Int -> Int . x 4) 3
              |                        ^
              |""".stripMargin
    }

    test("an Int cannot be passed to an Int -> Int (untyped)") {
        analyse("""(\x . x 4) 3""") shouldBe empty
    }

    test("an Int -> Int cannot be passed to an Int (typed)") {
        analyse("""(\x : Int . x + x) (\y : Int . y + 1)""") shouldBe
            """1:21:error: expected Int, found Int -> Int
              |(\x : Int . x + x) (\y : Int . y + 1)
              |                    ^
              |""".stripMargin
    }

    test("an Int -> Int cannot be passed to an Int (untyped)") {
        analyse("""(\x . x + x) (\y . y + 1)""") shouldBe empty
    }

    test("an Int cannot be directly applied as a function") {
        analyse("""1 3""") shouldBe
            """1:1:error: application of non-function
              |1 3
              |^
              |""".stripMargin
    }

    test("an Int cannot be applied as a function via a parameter (typed)") {
        analyse("""(\x : Int . x 5) 7""") shouldBe
            """1:13:error: application of non-function
              |(\x : Int . x 5) 7
              |            ^
              |""".stripMargin
    }

    test("an Int cannot be applied as a function via a parameter (untyped)") {
        analyse("""(\x . x 5) 7""") shouldBe empty
    }

    /**
     * Canonicalise an expression so that its binding variable names
     * are given by the depth of their binder in the whole expression.
     * Unbound vars are not changed.
     */
    def canon(x : Exp) : Exp = {
        def canons(d : Int, e : Map[Idn, Idn]) : Strategy =
            rule[Exp] {
                case Var(n) =>
                    Var(e(n))
                case Lam(n, t, b) =>
                    val m = s"v${d.toString}"
                    Lam(m, t, canonise(b, d + 1, e + (n -> m)))
                case Let(n, t, e2, e1) =>
                    val m = s"v${d.toString}"
                    Let(m, t, canonise(e2, d + 1, e), canonise(e1, d + 1, e + (n -> m)))
            } +
                rwall(canons(d, e))
        def canonise(x : Exp, d : Int, e : Map[Idn, Idn]) : Exp =
            rewrite(canons(d, e))(x)
        canonise(x, 1, Map() withDefault (n => n))
    }

    /**
     * Parse and evalaute the term string using the given evaluator. If it works,
     * return the result of evaluation. If* the term fails to parse return the error
     * message.
     */
    def eval(term : String, evaluator : Evaluator) : Either[String, Exp] = {
        parsers.parseAll(parsers.exp, StringSource(term)) match {
            case Success(exp, _) =>
                Right(evaluator.eval(exp))
            case n : NoSuccess =>
                Left(n.message)
        }
    }

    // FIXME deal with other evaluation mechanisms

    /**
     * Matcher for evaluation where `mech` specifies the evaluation mechanism to use.
     * The result of evaluation and the expected results are canonicalised before
     * comparison. `expected1` is used if the evaluator evaluates inside lambdas, otherwise `expected2` is used.
     */
    def evalTo(mech : String, expected1 : Exp, expected2 : Exp) =
        new Matcher[String] {
            def apply(term : String) = {
                val evaluator = evaluatorFor(mech)
                eval(term, evaluator) match {
                    case Left(msg) =>
                        MatchResult(
                            false,
                            s""""$mech: $term" failed to parse: "$msg"""",
                            "NOT USED"
                        )
                    case Right(result) =>
                        val expected = if (evaluator.reducesinlambdas) expected1 else expected2
                        MatchResult(
                            canon(result) == canon(expected),
                            s""""$mech: $term" evaluated to $result not expected $expected""",
                            s""""$mech: $term" evaluated to $expected"""
                        )
                }
            }
        }

    /**
     * Matcher for evaluation that tries all of the evaluation mechanisms
     * using the same expected value for all mechanisms.
     */
    def evalAllTo(term : String, expected : Exp) =
        for (mech <- mechanisms) {
            term should evalTo(mech, expected, expected)
        }

    /**
     * Matcher for evaluation that tries all of the evaluation mechanisms
     * using `expected1` for mechanisms that evaluate inside lambdas and
     * `expected2` otherwise.
     */
    def evalAllToBoth(term : String, expected1 : Exp, expected2 : Exp) =
        for (mech <- mechanisms) {
            term should evalTo(mech, expected1, expected2)
        }

    test("a single digit number evaluates to itself") {
        evalAllTo("4", Num(4))
    }

    test("a two digit number evaluates to itself") {
        evalAllTo("25", Num(25))
    }

    test("a four digit number evaluates to itself") {
        evalAllTo("9876", Num(9876))
    }

    test("a single character variable evaluates to itself") {
        evalAllTo("v", Var("v"))
    }

    test("a two character variable evaluates to itself") {
        evalAllTo("var", Var("var"))
    }

    test("a variable whose name contains digits evaluates to itself") {
        evalAllTo("v45", Var("v45"))
    }

    test("primitives evaluate correctly: addition") {
        evalAllTo("4 + 1", Num(5))
    }

    test("primitives evaluate correctly: subtraction") {
        evalAllTo("20 - 12", Num(8))
    }

    test("primitives evaluate correctly: addition and subtraction") {
        evalAllTo("12 + 7 - 19", Num(0))
    }

    test("primitives evaluate correctly: addition and subtraction with parens") {
        evalAllTo("12 + (7 - 19)", Num(0))
    }

    test("primitives evaluate correctly: addition twice") {
        evalAllTo("2 + 3 + 4", Num(9))
    }

    test("primitives evaluate correctly: subtraction twice") {
        evalAllTo("2 - 3 - 4", Num(-5))
    }

    test("primitives evaluate correctly: subtraction twice with parens") {
        evalAllTo("2 - (3 - 4)", Num(3))
    }

    test("lambda expressions evaluate to themselves: constant body") {
        evalAllTo(
            """\x:Int.4""",
            Lam("x", IntType(), Num(4))
        )
    }

    test("lambda expressions evaluate to themselves: non-constant body") {
        evalAllTo(
            """\x : Int . x - 1""",
            Lam("x", IntType(), Opn(Var("x"), SubOp(), Num(1)))
        )
    }

    test("parameters are correctly substituted: integer param") {
        evalAllTo("""(\x : Int . x) 42""", Num(42))
    }

    test("parameters are correctly substituted: function param") {
        evalAllTo(
            """(\x : Int -> Int . x) (\y : Int . y)""",
            Lam("y", IntType(), Var("y"))
        )
    }

    test("a beta reduction and an operator evaluation works") {
        evalAllTo("""(\x . x + 1) 4""", Num(5))
    }

    test("an unused parameter is ignored: integer param") {
        evalAllTo("""(\x:Int.99)42""", Num(99))
    }

    test("an unused parameter is ignored: integer param with whitespace") {
        evalAllTo("""(\x : Int . 4 + 3) 8""", Num(7))
    }

    test("an unused parameter is ignored: function param") {
        evalAllTo("""(\x.99) (\y:Int.y)""", Num(99))
    }

    test("a function of one parameter passed as a parameter can be called") {
        evalAllTo(
            """(\f : Int -> Int . f 4) (\x : Int . x + 1)""",
            Num(5)
        )
    }

    test("a function of multiple parameters passed as a parameter can be called") {
        evalAllTo(
            """(\f : Int -> Int -> Int . f 1 2) (\x : Int . (\y : Int . x + y))""",
            Num(3)
        )
    }

    test("multiple parameters are passed correctly") {
        evalAllTo("""(\x . \f . f x) 4 (\y . y - 1)""", Num(3))
    }

    test("applications in arguments are evaluated correctly") {
        evalAllTo(
            """(\x . x + x) ((\y . y + 1) 5)""",
            Num(12)
        )
    }

    test("redexes inside lambdas are evaluated or ignored as appropriate") {
        evalAllToBoth("""\x:Int.4+3""", Lam("x", IntType(), Num(7)),
            Lam("x", IntType(), Opn(Num(4), AddOp(), Num(3))))
    }

    /**
     * Matcher for round-tripping a string through the parser and then
     * the result throgh the pretty-printer.
     */
    def roundTripTo(expected : String) =
        new Matcher[String] {
            def apply(term : String) =
                parsers.exp(term) match {
                    case Success(exp, _) =>
                        val layout = formattedLayout(exp)
                        MatchResult(
                            layout == expected,
                            s""""$term" round-tripped to "$layout not expected "$expected"""",
                            s""""$term" round-tripped to "$expected""""
                        )
                    case result : NoSuccess =>
                        MatchResult(false, s""""$term" ${result.toMessage}""", "NOT USED")
                }
        }

    test("pretty-print lambda expression, simple operation") {
        """\x:Int.x+1""" should roundTripTo("""(\x : Int . (x + 1))""")
    }

    test("pretty-print applications, nested operation") {
        """(\f:Int->Int.f 4)(\x:Int.x+x-5)""" should roundTripTo(
            """((\f : Int -> Int . (f 4)) (\x : Int . ((x + x) - 5)))"""
        )
    }

    test("pretty-printed nested lets") {
        formattedLayout(
            Let("a", IntType(),
                Let("b", IntType(), Num(1),
                    Let("c", IntType(), Num(1),
                        Let("d", IntType(), Num(1),
                            Let("e", IntType(), Num(1),
                                Let("f", IntType(), Num(1),
                                    Num(1)))))),
                Let("g", IntType(), Num(1),
                    Let("h", IntType(), Num(1),
                        Let("i", IntType(), Num(1),
                            Num(1)))))
        ) shouldBe
            """(let a : Int =
    (let b : Int =
        1 in
        (let c : Int =
            1 in
            (let d : Int =
                1 in
                (let e : Int =
                    1 in
                    (let f : Int =
                        1 in
                        1))))) in
    (let g : Int =
        1 in
        (let h : Int =
            1 in
            (let i : Int =
                1 in
                1))))"""

    }

    test("pretty-printed parallel lets") {
        formattedLayout(
            Letp(
                Vector(
                    Bind("a", Num(1)),
                    Bind("b", Num(1)),
                    Bind("c", Letp(
                        Vector(
                            Bind("d", Num(1)),
                            Bind("e", Num(1))
                        ),
                        Num(1)
                    ))
                ),
                Letp(
                    Vector(
                        Bind("f", Num(1)),
                        Bind("g", Num(1))
                    ),
                    Num(1)
                )
            )
        ) shouldBe
            """(letp
    a = 1
    b = 1
    c = (letp
        d = 1
        e = 1 in
        1) in
    (letp
        f = 1
        g = 1 in
        1))"""

    }

}
