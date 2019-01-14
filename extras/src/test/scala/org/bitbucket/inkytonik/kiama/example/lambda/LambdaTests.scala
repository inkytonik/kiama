/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.bitbucket.inkytonik.kiama.util.GeneratingREPL

/**
 * Lambda calculus tests.
 */
class LambdaTests extends ParseTests with Evaluator with Generator {

    import LambdaTree._
    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
    import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}
    import org.scalacheck.Prop._

    val positions = new Positions
    val parsers = new SyntaxAnalyser(positions)

    /**
     * Parse and evaluate the term string and if it works return the result
     * of the evaluation as an expression. It the term fails to parse,
     * return the error message.
     */
    def eval(term : String) : Either[String, Exp] = {
        parsers.parseAll(parsers.exp, StringSource(term)) match {
            case Success(exp, _) =>
                Right(evaluate(exp))
            case NoSuccess(msg, _) =>
                Left(msg)
        }
    }

    test("an integer leaf evaluates to itself") {
        check((i : Int) => (i >= 0) ==> (eval(i.toString) == Right(Num(i))))
    }

    test("a variable leaf evaluates to itself") {
        check((v : Var) => (eval(v.toString) == Right(v)))
    }

    test("a numeric parameter is passed and ignored") {
        eval("""(\x.99) 42""") shouldBe Right(Num(99))
    }

    test("a numeric parameter is passed and substituted") {
        eval("""(\x.x) 42""") shouldBe Right(Num(42))
    }

    test("recursive application works") {
        eval("""(\y.y) (\x.x) 42""") shouldBe Right(Num(42))
    }

    test("another recursive application works") {
        eval("""(\y.y) (\x.99) 42""") shouldBe Right(Num(99))
    }

    test("name capturing is avoided (1)") {
        eval("""(\x.x) x""") shouldBe Right(Var("x"))
    }

    test("name capturing is avoided (2)") {
        eval("""(\x.\y.x) y""") shouldBe Right(Lam("_v0", Var("y")))
    }

    test("name capturing is avoided (3)") {
        eval("""(\x. x y) (\x. y x)""") shouldBe Right(App(Var("y"), Var("y")))
    }

    test("a function parameter is passed and ignored") {
        eval("""(\x.99) (\y.y)""") shouldBe Right(Num(99))
    }

    test("a function parameter is passed and substituted") {
        eval("""(\x.x) (\y.y)""") shouldBe Right(Lam("y", Var("y")))
    }

    test("a variable is substituted at multiple levels") {
        eval("""(\y.(\z.z) y) x""") shouldBe Right(Var("x"))
    }

    /**
     *     true = \x.\y.x
     *    false = \x.\y.y
     *  if-then-else = \a.\b.\c.((a)b)c
     *  (((if-then-else)false)42)99 -> 99
     */
    test("Church encodings of Booleans work") {
        eval("""(((\a.\b.\c.((a)b)c) (\x.\y.y)) 42) 99""") shouldBe Right(Num(99))
    }

    test("a syntactically invalid term produces a parse error") {
        eval("""(\x ,99 42""") shouldBe Left("'.' expected but ',' found")
    }

}

/**
 * ScalaCheck generators for programs in the lambda language.
 */
trait Generator {

    import org.scalacheck._
    import LambdaTree._

    val genNum = for (i <- Gen.choose(1, 100)) yield Num(i)
    val genIdn : Gen[String] = for (s <- Gen.identifier) yield (s.take(5))
    val genVar = for (v <- genIdn) yield Var(v)

    implicit def arbVar : Arbitrary[Var] = Arbitrary(genVar)

    val genLeafExp = Gen.oneOf(genNum, genVar)

    def genLamExp(sz : Int) : Gen[Lam] =
        for { i <- genIdn; b <- genExp(sz / 2) } yield Lam(i, b)

    def genAppExp(sz : Int) : Gen[App] =
        for { l <- genExp(sz / 2); r <- genExp(sz / 2) } yield App(l, r)

    def genExp(sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency((1, genLeafExp), (1, genLamExp(sz)), (3, genAppExp(sz)))

    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized(sz => genExp(sz)) }

}

/**
 * A read-eval-print loop for generating random expressions.
 */
object LambdaGen extends GeneratingREPL[LambdaTree.Exp] with Generator {

    import org.scalacheck.Arbitrary

    def generator : Arbitrary[LambdaTree.Exp] =
        arbExp

}
