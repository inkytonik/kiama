/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009 Anthony M Sloane, Macquarie University.
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

package kiama.example.lambda2

import junit.framework.Assert._
import kiama.rewriting.Rewriter
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers

import AST._

/**
 * Lambda calculus tests.
 */
class LambdaTests extends JUnit3Suite with Checkers with Parser with Rewriter {

    import Analysis._
    import Evaluators._
    import kiama.attribution.Attribution._
    import kiama.util.Messaging._
    import scala.collection.mutable.HashMap
    import scala.util.parsing.input.CharArrayReader

    /**
     * Compute the type of e using the specified attribute and check to make
     * sure the relevant message is reported.
     */
    def assertType (e : Exp, aname : String, a : Exp ==> Type, line : Int, col : Int, msg : String) {
        a (e)
        if (messagecount == 0)
            fail (aname + ": no messages produced, expected (" + line + "," + col + ") " + msg)
        else {
            val m = messages (0)
            if ((m.pos.line != line) || (m.pos.column != col) ||
                (m.message != msg))
                fail (aname + ": incorrect message, expected (" + line + "," + col + ") " + msg +
                      ", got (" + m.pos.line + "," + m.pos.column + ") " + m.message)
        }
        resetmessages
    }

    /**
     * Compute the tipe of the expression and check to see if the specified
     * message is produced.  We test both of the analysis methods.
     */
    def assertMessage (term : String, line : Int, col : Int, msg : String) {
        val in = new CharArrayReader (term.toArray)
        parse (in) match {
            case Success (e, in) if in.atEnd =>
                assertType (e, "tipe", tipe, line, col, msg)
                assertType (e, "tipe2", tipe2, line, col, msg)
            case Success (_, in) =>
                fail ("extraneous input at " + in.pos + ": " + term)
            case f @ Failure (_, _) =>
                fail ("parse failure: " + f)
        }
    }

    /**
     * Test analysis of names in expressions.
     */
    def testNameAnalysis () {
        assertMessage ("""\x : Int . x + y""", 1, 16, "'y' unknown")
        assertMessage ("""(\x : Int -> Int . x + 1) (\y : Int . y)""", 1, 20,
                       "expected Int, found Int -> Int")
        assertMessage ("""(\x : Int -> Int . x 4) 3""", 1, 25, "expected Int -> Int, found Int")
        assertMessage ("""(\x : Int . x + x) (\y : Int . y + 1)""", 1, 21,
                       "expected Int, found Int -> Int")
        assertMessage ("""1 3""", 1, 1, "application of non-function")
        assertMessage ("""(\x : Int . x 5) 7""", 1, 13, "application of non-function")
    }

    /**
     * Canonicalise an expression so that its binding variable names
     * are given by the depth of their binder in the whole expression.
     * Unbound vars are not changed.
     */
    def canon (x : Exp) : Exp = {
        def canons (d : Int, e : Map[Idn,Idn]) : Strategy =
            rule {
                case Var (n)            =>
                    Var (e (n))
                case Lam (n, t, b)      =>
                    val m = "v" + d.toString
                    Lam (m, t, canonise (b, d + 1, e + (n->m)))
                case Let (n, t, e2, e1) =>
                    val m = "v" + d.toString
                    Let (m, t, canonise (e2, d + 1, e), canonise (e1, d + 1, e + (n->m)))
            } +
            all (canons (d, e))
        def canonise (x : Exp, d : Int, e : Map[Idn,Idn]) : Exp =
           rewrite (canons (d, e)) (x)
        canonise (x, 1, Map () withDefault (n => n))
    }

    /**
     * Assert true if the two expressions are the same modulo variable
     * renaming, otherwise assert an equality failure.  Make sure to
     * make a failure assertion using the mechanism name on the original
     * expressions so that it makes sense to the user.
     */
    def assertSame (mech : String, e1 : Exp, e2 : Exp) =
        if (canon (e1) != canon (e2))
            assertEquals (mech, e1, e2)

    /**
     * Parse and evaluate term using the specified mechanism
     * (which is assumed to already have been set) then compare to
     * result. Fail if the parsing fails or the comparison with
     * the result fails.
     */
    def assertEval (mech : String, term : String, result : Exp) {
        val in = new CharArrayReader (term.toArray)
        parse (in) match {
            case Success (e, in) if in.atEnd =>
                val r = evaluator.eval (e)
                assertSame (mech, result, r)
            case Success (_, in) =>
                fail ("extraneous input at " + in.pos + ": " + term)
            case f @ Failure (_, _) =>
                fail ("parse failure: " + f)
        }
    }

    /**
     * Test the assertion on all available evaluation mechanisms.
     */
    def assertEvalAll (term : String, result : Exp) {
        for (mech <- mechanisms) {
            setEvaluator (mech)
            assertEval (mech, term, result)
        }
    }

    /**
     * Test the assertion on all available evaluation mechanisms.
     * Same as single result version, except that result1 is
     * expected for mecahnisms that evaluate inside lambdas and
     * result2 is expected for those that don't.
     */
    def assertEvalAll (term : String, result1 : Exp, result2 : Exp) {
        for (mech <- mechanisms) {
            setEvaluator (mech)
            if (evaluator reducesinlambdas)
              assertEval (mech, term, result1)
            else
              assertEval (mech, term, result2)
        }
    }

    /**
     * Test expressions with no sub-structure.
     */
    def testLeaves () {
        assertEvalAll ("4", Num (4))
        assertEvalAll ("25", Num (25))
        assertEvalAll ("9876", Num (9876))
        assertEvalAll ("v", Var ("v"))
        assertEvalAll ("var", Var ("var"))
        assertEvalAll ("v45", Var ("v45"))
    }

    /**
     * Test expressions involving only primitive operations.
     */
    def testPrimitives () {
        assertEvalAll ("4 + 1", Num (5))
    }

    /**
     * Test lambda expressions with no reduction.
     */
    def testLambda () {
        assertEvalAll ("""\x:Int.4""",
                       Lam ("x", IntType, Num (4)))
        assertEvalAll ("""\x : Int . x - 1""",
                       Lam ("x", IntType, Opn (SubOp, Var ("x"), Num (1))))
    }

    /**
     * Tests only requiring a small number of reductions.
     */
    def testSimple () {
        assertEvalAll ("""(\x : Int . x) 42""", Num (42))
        assertEvalAll ("""(\x : Int . x + 1) 4""", Num (5))
        assertEvalAll ("""(\x:Int.99)42""", Num (99))
        assertEvalAll ("""(\x : Int . 4 + 3) 8""", Num (7))
        assertEvalAll ("""(\x:Int->Int.99) (\y:Int.y)""", Num (99))
        assertEvalAll ("""(\x : Int -> Int . x) (\y : Int . y)""",
                       Lam ("y", IntType, Var ("y")))
    }

    /**
     * Tests invlving more complex reduction patterns.
     */
    def testComplex () {
        assertEvalAll ("""(\f : Int -> Int . f 4) (\x : Int . x + 1)""",
                       Num (5))
        assertEvalAll ("""(\f : Int -> Int -> Int . f 1 2) (\x : Int . (\y : Int . x + y))""",
                       Num (3))
        assertEvalAll ("""(\x : Int . \f : Int -> Int . f x) 4 (\y : Int . y - 1)""",
                       Num (3))
        assertEvalAll ("""(\x : Int . x + x) ((\y : Int . y + 1) 5)""",
                       Num (12))
    }

    /**
     * Tests whose outcomes depend on whether we reduce inside lambdas or not.
     */
    def testVariants () {
        assertEvalAll ("""\x:Int.4+3""", Lam ("x", IntType, Num (7)),
                       Lam ("x", IntType, Opn (AddOp, Num (4), Num (3))))
    }

}
