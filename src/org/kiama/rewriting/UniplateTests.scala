/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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
package rewriting

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.kiama.example.imperative.Generator

/**
 * Tests based on examples from the paper "Uniform boilerplate and list
 * processing" by Mitchell and Runciman, from Haskell Workshop 2007.
 */
@RunWith(classOf[JUnitRunner])
class UniplateTests extends FunSuite with Checkers with Generator {

    import org.kiama.example.imperative.AST._
    import org.kiama.rewriting.Rewriter._
    import org.scalacheck._
    import org.scalacheck.Prop._

    /**
     * A simple numeric test expression.
     */
    val numexp = Num (42)

    /**
     * A simple test expression involving variables.
     */
    val varexp = Div (Mul (Var ("var1"), Var ("var2")), Var ("var1"))

    test ("collection of variable references: direct style") {
        /**
         *  Direct style: local management of the collection.
         */
        def variables (e : Exp) : Set[String] = {
            var vars = Set[String]()
            everywheretd (query { case Var (s) => vars += s }) (e)
            vars
        }
        check ((e : Exp) => variables (e) == e.vars)
    }

    {
        val variabless = collects { case Var (s) => s }

        test ("collection of variable references: indirect style") {
            // Indirect: using the collects combinator to manage the set
            check ((e : Exp) => variabless (e) == e.vars)
        }

        test ("collection of variable references: indirect style on sets and lists") {
            // Simple check of set and list versions of collect
            val variablesl = collectl { case Var (s) => s }
            expect (Set ()) (variabless (numexp))
            expect (List ()) (variablesl (numexp))
            expect (Set ("var1", "var2")) (variabless (varexp))
            expect (List ("var1", "var2", "var1")) (variablesl (varexp))
        }
    }

    test ("search for division by zero") {
        object TestDivsByZero extends Generator {
            override def genDiv (sz : Int) =
                Gen.frequency ((1, genDivByZero (sz)), (1, super.genDiv (sz)))
            def genDivByZero (sz : Int) =
                for { l <- genExp (sz/2) } yield Div (l, Num (0))
            val divsbyzero = count { case Div (_, Num (0)) => 1 }
            expect (0) (divsbyzero (numexp))
            expect (0) (divsbyzero (varexp))
            check ((e : Exp) => (e.divsbyzero != 0) ==> (divsbyzero (e) == e.divsbyzero))
        }
        TestDivsByZero
    }

    test ("arithmetic simplification") {
        def simplify : Exp => Exp =
            rewrite (everywheretd (rule {
                case Sub (x, y)           => simplify (Add (x, Neg (y)))
                case Add (x, y) if x == y => Mul (Num (2), x)
            }))
        expect (numexp) (simplify (numexp))
        expect (varexp) (simplify (varexp))

        val e = Sub (Add (Var ("a"), Var ("a")),
                     Add (Sub (Var ("b"), Num (1)), Sub (Var ("b"), Num (1))))
        val simpe = Add (Mul (Num (2), Var ("a")),
                         Neg (Mul (Num (2), Add (Var ("b"), Neg (Num (1))))))
        expect (simpe) (simplify (e))

        val f = Sub (Neg (Num (1)), Num (1))
        val simpf = Mul (Num (2), Neg (Num (1)))
        expect (simpf) (simplify (f))

        check ((e : Exp) => simplify (e).value == e.value)
    }

    test ("remove double negations") {
        object TestDoubleNegSimplification extends Generator {
            override def genNeg (sz : Int) =
                Gen.frequency ((1, genDoubleNeg (sz)), (1, super.genNeg (sz)))
            def genDoubleNeg (sz : Int) =
                for { e <- super.genNeg (sz) } yield Neg (e)
            def doubleneg : Exp => Exp =
                rewrite (everywherebu ( rule { case Neg (Neg (x)) => x }))
            expect (numexp) (doubleneg (numexp))
            expect (varexp) (doubleneg (varexp))
            check ((e : Exp) => doubleneg (e).value == e.value)
        }
        TestDoubleNegSimplification
    }

    test ("reciprocal division to multiplication conversion") {
        def reciprocal : Exp => Exp =
            rewrite (everywherebu (rule {
                case Div (n, m) => Mul (n, Div (Num (1), m))
            }))

        val e1 = Div (Num (1), Num (2))
        expect (0.5) (reciprocal (e1).value)

        val e2 = Mul (Num (2), Div (Num (3), Num (4)))
        expect (1.5) (reciprocal (e2).value)
    }

    test ("unique variable renaming") {
        def uniquevars : Exp => Exp =
            rewrite ({
                var count = 0
                everywheretd (rule { case Var (s) => count = count + 1; Var ("x" + count) })
            })
        expect (numexp) (uniquevars (numexp))
        // Run this twice to make sure that count is not shared
        expect (Div (Mul (Var ("x1"), Var ("x2")), Var ("x3"))) (uniquevars (varexp))
        expect (Div (Mul (Var ("x1"), Var ("x2")), Var ("x3"))) (uniquevars (varexp))
        check ((e : Exp) => uniquevars (e).value == e.value)
    }

    test ("calculate expression depth") {
        def maximum (l : Seq[Int]) : Int = l.drop (1).foldLeft (l.head)(_.max(_))
        val depth = para ((t : Any, cs : Seq[Int]) => 1 + maximum (List (0) ++ cs))
        expect (2) (depth (numexp))
        expect (4) (depth (varexp))
        check ((e : Exp) => depth (e) == e.depth)
    }

    test ("variable renaming") {
        def rename : Exp => Exp =
            rewrite (everywheretd (rule { case Var (s) => Var ("_" + s) }))
        check ((e : Exp) => rename (e).vars == e.vars.map ("_" + _))
    }

    test ("optimisation of integer addition") {
        object OptimiseAdd extends Generator {
            override def genAdd (sz : Int) =
                Gen.frequency ((1, genIntAdd (sz)), (1, super.genAdd (sz)))
            def genIntAdd (sz : Int) =
                for { l <- genNum; r <- genNum } yield Add (l, r)
            def optimiseadd : Exp => Exp =
                rewrite (everywherebu (rule {
                    case Add (Num (n), Num (m)) => Num (n + m)
                }))
            check ((e : Exp) => {
                val eopt = optimiseadd (e)
                (eopt.intadds == 0) && (eopt.value == e.value)
            })
        }
        OptimiseAdd
    }

}
