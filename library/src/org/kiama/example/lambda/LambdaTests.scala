/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package example.lambda

import org.kiama.util.GeneratingREPL
import org.kiama.util.Tests
import org.scalatest.prop.Checkers

/**
 * Lambda calculus tests.
 */
class LambdaTests extends Tests with Checkers with Parser with Evaluator
        with Generator {

    import AST._
    import org.scalacheck.Prop._

    /**
     * Parse and evaluate term then compare to result. Fail if any the parsing
     * or the comparison fail.
     */
    def expectEval (term : String, result : Exp) {
        parseAll (parser, term) match {
            case Success (e, in) if in.atEnd =>
                normal (e) match {
                    case Some (r) => assertResult (result) (r)
                    case None     => fail (s"reduction failed: $term")
                }
            case Success (_, in) =>
                fail (s"extraneous input at ${in.pos}: $term")
            case f =>
                fail (s"parse failure: $f")
        }
    }

   /**
     * Parse and evaluate term then compare to expected result. Fail if the
     * parsing or the comparison fail.
     */
    def evalTo (term : String, result : Exp) : Boolean = {
        parseAll (parser, term) match {
            case Success (e, in) if in.atEnd =>
                normal (e) match {
                    case Some (r) => r == result
                    case None     => false
                }
            case _ =>
                false
        }
    }

    test ("an integer leaf evaluates to itself") {
        check ((i : Int) => (i >= 0) ==> evalTo (i.toString, Num (i)))
    }

    test ("a variable leaf evaluates to itself") {
        check ((v : Var) => evalTo (v.toString, v))
    }

    test ("a numeric parameter is passed and ignored") {
        expectEval ("""(\x.99) 42""", Num (99))
    }

    test ("a numeric parameter is passed and substituted") {
        expectEval ("""(\x.x) 42""", Num (42))
    }

    test ("a function parameter is passed and ignored") {
        expectEval ("""(\x.99) (\y.y)""", Num (99))
    }

    test ("a function parameter is passed and substituted") {
        expectEval ("""(\x.x) (\y.y)""", Lam ("y", Var ("y")))
    }

    test ("a variable is substituted at multiple levels") {
        expectEval ("""(\y.(\z.z) y) x""", Var ("x"))
    }

    /**
     *     true = \x.\y.x
     *    false = \x.\y.y
     *  if-then-else = \a.\b.\c.((a)b)c
     *  (((if-then-else)false)42)99 -> 99
     */
    test ("Church encodings of Booleans work") {
        expectEval ("""(((\a.\b.\c.((a)b)c) (\x.\y.y)) 42) 99""", Num (99))
    }

}

/**
 * ScalaCheck generators for programs in the lambda language.
 */
trait Generator {

    import org.scalacheck._
    import AST._

    val genNum = for (i <- Gen.choose (1, 100)) yield Num (i)
    val genIdn : Gen[String] = for (s <- Gen.identifier) yield (s.take (5))
    val genVar = for (v <- genIdn) yield Var (v)

    implicit def arbVar : Arbitrary[Var] = Arbitrary (genVar)

    val genLeafExp = Gen.oneOf (genNum, genVar)

    def genLamExp (sz : Int) : Gen[Lam] =
        for { i <- genIdn; b <- genExp (sz/2) } yield Lam (i, b)

    def genAppExp (sz : Int) : Gen[App] =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield App (l, r)

    def genExp (sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency ((1, genLeafExp), (1, genLamExp (sz)), (3, genAppExp (sz)))

    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized (sz => genExp (sz)) }

}

/**
 * A read-eval-print loop for generating random expressions.
 */
object LambdaGen extends GeneratingREPL[AST.Exp] with Generator {

    import org.scalacheck.Arbitrary

    def generator : Arbitrary[AST.Exp] =
        arbExp

}
