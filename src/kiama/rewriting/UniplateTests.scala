/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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

package kiama.rewriting

import junit.framework.Assert._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
import kiama.example.imperative.TestBase

/**
 * Tests based on examples from the paper "Uniform boilerplate and list
 * processing" by Mitchell and Runciman, from Haskell Workshop 2007.
 */
class UniplateTests extends JUnit3Suite with Checkers with Rewriter
                    with TestBase {

    import kiama.example.imperative.AST._

    /**
     * A simple numeric test expression.
     */
    val numexp = Num (42)

    /**
     * A simple test expression involving variables.
     */
    val varexp = Div (Mul (Var ("var1"), Var ("var2")), Var ("var1"))

    /**
     * Tests that collect variable references.
     */
    def testVariables () {
        /**
         *  Direct style: local management of the collection.
         */
        def variables (e : Exp) : Set[String] = {
            var vars = Set[String]()
            everywheretd (query { case Var (s) => vars += s }) (e)
            vars
        }
        check ((e : Exp) => variables (e) == e.vars)

        // Indirect: using the collects combinator to manage the set
        val variabless = collects { case Var (s) => s }
        check ((e : Exp) => variabless (e) == e.vars)

        // Simple check of set and list versions of collect
        val variablesl = collectl { case Var (s) => s }
        assertEquals (Set (), variabless (numexp))
        assertEquals (List (), variablesl (numexp))
        assertEquals (Set ("var1", "var2"), variabless (varexp))
        assertEquals (List ("var1", "var2", "var1"), variablesl (varexp))
    }

    /**
     * Tests that search for divisions by literal zero.
     */
    def testDivsByZero () {
        object TestDivsByZero extends TestBase {
            override def genDiv (sz : Int) =
                Gen.frequency ((1, genDivByZero (sz)), (1, super.genDiv (sz)))
            def genDivByZero (sz : Int) =
                for { l <- genExp (sz/2) } yield Div (l, Num (0))
            def divsbyzero = count { case Div (_, Num (0)) => 1 }
            assertEquals (0, divsbyzero (numexp))
            assertEquals (0, divsbyzero (varexp))
            check ((e : Exp) => (e.divsbyzero != 0) ==> (divsbyzero (e) == e.divsbyzero))
        }
        TestDivsByZero ()
    }

    /**
     * Tests of arithmetic simplification transformations.
     */
    def testSimplification () {
        def simplify : Exp => Exp =
            rewrite (everywheretd (rule {
                case Sub (x, y)           => simplify (Add (x, Neg (y)))
                case Add (x, y) if x == y => Mul (Num (2), x)
            }))
        assertEquals (numexp, simplify (numexp))
        assertEquals (varexp, simplify (varexp))

        val e = Sub (Add (Var ("a"), Var ("a")),
                     Add (Sub (Var ("b"), Num (1)), Sub (Var ("b"), Num (1))))
        val simpe = Add (Mul (Num (2), Var ("a")),
                         Neg (Mul (Num (2), Add (Var ("b"), Neg (Num (1))))))
        assertEquals (simpe, simplify (e))

        val f = Sub (Neg (Num (1)), Num (1))
        val simpf = Mul (Num (2), Neg (Num (1)))
        assertEquals (simpf, simplify (f))

        check ((e : Exp) => simplify (e).value == e.value)
    }

    /**
     * Tests that remove double negations.
     */
    def testDoubleNegSimplification () {
        object TestDoubleNegSimplification extends TestBase {
            override def genNeg (sz : Int) =
                Gen.frequency ((1, genDoubleNeg (sz)), (1, super.genNeg (sz)))
            def genDoubleNeg (sz : Int) =
                for { e <- super.genNeg (sz) } yield Neg (e)
            def doubleneg : Exp => Exp =
                rewrite (everywherebu ( rule { case Neg (Neg (x)) => x }))
            assertEquals (numexp, doubleneg (numexp))
            assertEquals (varexp, doubleneg (varexp))
            check ((e : Exp) => doubleneg (e).value == e.value)
        }
        TestDoubleNegSimplification ()
    }

    /**
     * Tests of reciprocal division conversion to multiplication.
     */
    def testReciprocal () {
        def reciprocal : Exp => Exp =
            rewrite (everywherebu (rule {
                case Div (n, m) => Mul (n, Div (Num (1), m))
            }))

        val e1 = Div (Num (1), Num (2))
        assertEquals (0.5, reciprocal (e1).value)

        val e2 = Mul (Num (2), Div (Num (3), Num (4)))
        assertEquals (1.5, reciprocal (e2).value)
    }

    /**
     * Tests that rename variables to be unique
     */
    def testUniqueVars () {
        def uniquevars : Exp => Exp =
            rewrite ({
                var count = 0
                everywheretd (rule { case Var (s) => count = count + 1; Var ("x" + count) })
            })
        assertEquals (numexp, uniquevars (numexp))
        // Run this twice to make sure that count is not shared
        assertEquals (Div (Mul (Var ("x1"), Var ("x2")), Var ("x3")), uniquevars (varexp))
        assertEquals (Div (Mul (Var ("x1"), Var ("x2")), Var ("x3")), uniquevars (varexp))
        check ((e : Exp) => uniquevars (e).value == e.value)
    }

    /**
     * Tests that calculate expression depth.
     */
    def testDepth () {
        def maximum (l : Seq[Int]) : Int = l.drop (1).foldLeft (l.first)(_.max(_))
        def depth = para ((t : Any, cs : Seq[Int]) => 1 + maximum (List (0) ++ cs))
        assertEquals (2, depth (numexp))
        assertEquals (4, depth (varexp))
        check ((e : Exp) => depth (e) == e.depth)
    }

    /**
     * Tests that rename variables.  Note that in the Uniplate paper and the Compos
     * paper before it, this is a multi-type example, but we don't bother since many
     * of the other rewriting tests are multi-type.
     */
    def testRenameVar () {
        def rename : Exp => Exp =
            rewrite (everywheretd (rule { case Var (s) => Var ("_" + s) }))
        check ((e : Exp) => rename (e).vars == e.vars.map ("_" + _))
    }

    /**
     * Test optimisation of integer addition.
     */
    def testOptimiseAdd () {
        object OptimiseAdd extends TestBase {
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
        OptimiseAdd ()
    }

}
