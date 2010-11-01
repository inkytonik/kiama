/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
package example.lambda2

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
 * Lambda calculus tests.
 */
@RunWith(classOf[JUnitRunner])
class LambdaTests extends FunSuite with Checkers with Parser {

    import AST._
    import Analysis._
    import Evaluators._
    import org.kiama.attribution.Attribution._
    import org.kiama.rewriting.Rewriter._
    import org.kiama.util.Messaging._
    import org.scalacheck._
    import org.scalacheck.Prop.{all => _, _}
    import scala.collection.mutable.HashMap

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
    }

    /**
     * Compute the tipe of the expression and check to see if the specified
     * message is produced.  We test both of the analysis methods.
     */
    def assertMessage (term : String, line : Int, col : Int, msg : String) {
        resetmessages
        parseAll (start, term) match {
            case Success (e, in) if in.atEnd =>
                assertType (e, "tipe", tipe, line, col, msg)
                assertType (e, "tipe2", tipe2, line, col, msg)
            case Success (_, in) =>
                fail ("extraneous input at " + in.pos + ": " + term)
            case f =>
                fail ("parse failure: " + f)
        }
    }

    test ("an unknown variable by itself is reported") {
        assertMessage ("y", 1, 1, "'y' unknown")
    }

    test ("an unknown variable in an abstraction is reported") {
        assertMessage ("""\x : Int . x + y""", 1, 16, "'y' unknown")
    }

    test ("an Int -> Int cannot be used as an Int") {
        assertMessage ("""(\x : Int -> Int . x + 1) (\y : Int . y)""", 1, 20,
                       "expected Int, found Int -> Int")
    }

    test ("an Int cannot be passed to an Int -> Int") {
        assertMessage ("""(\x : Int -> Int . x 4) 3""", 1, 25,
                       "expected Int -> Int, found Int")
    }

    test ("an Int -> Int cannot be passed to an Int") {
        assertMessage ("""(\x : Int . x + x) (\y : Int . y + 1)""", 1, 21,
                       "expected Int, found Int -> Int")
    }

    test ("an Int cannot be directly applied as a function") {
        assertMessage ("""1 3""", 1, 1, "application of non-function")
    }

    test ("an Int cannot be applied as a function via a parameter") {
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
     * renaming, otherwise assert a failure.
     */
    def assertSame (mech : String, e1 : Exp, e2 : Exp) =
        if (canon (e1) != canon (e2))
            fail (mech + ": " + e1 + " and " + e2 + " are not equal")

    /**
     * Parse and evaluate term using the specified mechanism
     * (which is assumed to already have been set) then compare to
     * result. Fail if the parsing fails or the comparison with
     * the result fails.
     */
    def assertEval (mech : String, term : String, result : Exp) {
        parseAll (start, term) match {
            case Success (e, in) if in.atEnd =>
                val r = evaluator.eval (e)
                assertSame (mech, result, r)
            case Success (_, in) =>
                fail ("extraneous input at " + in.pos + ": " + term)
            case f =>
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
            assertEval (mech, term,
                        if (evaluator reducesinlambdas)
                            result1
                        else
                            result2)
        }
    }

    test ("a single digit number evaluates to itself") {
        assertEvalAll ("4", Num (4))
    }

    test ("a two digit number evaluates to itself") {
        assertEvalAll ("25", Num (25))
    }

    test ("a four digit number evaluates to itself") {
        assertEvalAll ("9876", Num (9876))
    }

    test ("a single character variable evaluates to itself") {
        assertEvalAll ("v", Var ("v"))
    }

    test ("a two character variable evaluates to itself") {
        assertEvalAll ("var", Var ("var"))
    }

    test ("a variable whose name contains digits evaluates to itself") {
        assertEvalAll ("v45", Var ("v45"))
    }

    test ("primitives evaluate correctly: addition") {
        assertEvalAll ("4 + 1", Num (5))
    }

    test ("primitives evaluate correctly: subtraction") {
        assertEvalAll ("20 - 12", Num (8))
    }

    test ("primitives evaluate correctly: addition and subtraction") {
        assertEvalAll ("12 + 7 - 19", Num (0))
    }

    test ("primitives evaluate correctly: addition and subtraction with parens") {
        assertEvalAll ("12 + (7 - 19)", Num (0))
    }

    test ("primitives evaluate correctly: addition twice") {
        assertEvalAll ("2 + 3 + 4", Num (9))
    }

    test ("primitives evaluate correctly: subtraction twice") {
        assertEvalAll ("2 - 3 - 4", Num (-5))
    }

    test ("primitives evaluate correctly: subtraction twice with parens") {
        assertEvalAll ("2 - (3 - 4)", Num (3))
    }

    test ("lambda expressions evaluate to themselves: constant body") {
        assertEvalAll ("""\x:Int.4""",
                       Lam ("x", IntType, Num (4)))
    }

    test ("lambda expressions evaluate to themselves: non-constant body") {
        assertEvalAll ("""\x : Int . x - 1""",
                       Lam ("x", IntType, Opn (SubOp, Var ("x"), Num (1))))
    }

    test ("parameters are correctly substituted: integer param") {
        assertEvalAll ("""(\x : Int . x) 42""", Num (42))
    }

    test ("parameters are correctly substituted: function param") {
        assertEvalAll ("""(\x : Int -> Int . x) (\y : Int . y)""",
                       Lam ("y", IntType, Var ("y")))
    }

    test ("a beta reduction and an operator evaluation works") {
        assertEvalAll ("""(\x : Int . x + 1) 4""", Num (5))
    }

    test ("an unused parameter is ignored: integer param") {
        assertEvalAll ("""(\x:Int.99)42""", Num (99))
    }

    test ("an unused parameter is ignored: integer param with whitespace") {
        assertEvalAll ("""(\x : Int . 4 + 3) 8""", Num (7))
    }

    test ("an unused parameter is ignored: function param") {
        assertEvalAll ("""(\x:Int->Int.99) (\y:Int.y)""", Num (99))
    }

    test ("a function of one parameter passed as a parameter can be called") {
        assertEvalAll ("""(\f : Int -> Int . f 4) (\x : Int . x + 1)""",
                       Num (5))
    }

    test ("a function of multiple parameters passed as a parameter can be called") {
        assertEvalAll ("""(\f : Int -> Int -> Int . f 1 2) (\x : Int . (\y : Int . x + y))""",
                       Num (3))
    }

    test ("multiple parameters are passed correctly") {
        assertEvalAll ("""(\x : Int . \f : Int -> Int . f x) 4 (\y : Int . y - 1)""",
                       Num (3))
    }

    test ("applications in arguments are evaluated correctly") {
        assertEvalAll ("""(\x : Int . x + x) ((\y : Int . y + 1) 5)""",
                       Num (12))
    }

    test ("redexes inside lambdas are evaluated or ignored as appropriate") {
        assertEvalAll ("""\x:Int.4+3""", Lam ("x", IntType, Num (7)),
                       Lam ("x", IntType, Opn (AddOp, Num (4), Num (3))))
    }

}
