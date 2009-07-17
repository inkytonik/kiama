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

package kiama.example.lambda

import junit.framework.Assert._
import junit.framework.TestCase
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers

/**
 * Lambda calculus tests.
 */
class LambdaTests extends TestCase with JUnit3Suite with Checkers
                  with TestBase {

    import AST._
    import scala.util.parsing.input.CharArrayReader

    /**
     * Parse and evaluate term then compare to result. Fail if any the parsing
     * or the comparison fail.  This is the JUnit version.
     */
    def assertEval (term : String, result : Exp) {
        val in = new CharArrayReader (term.toArray)
        parse (in) match {
            case Success (e, in) if in.atEnd =>
                normal (e) match {
                    case Some (r) => assertEquals (result, r)
                    case None     => fail ("reduction failed: " + term)
                }
            case Success (_, in) =>
                fail ("extraneous input at " + in.pos + ": " + term)
            case f @ Failure (_, _) =>
                fail ("parse failure: " + f)
        }
    }

   /**
     * Parse and evaluate term then compare to result. Fail if any the parsing
     * or the comparison fail.  This is the Boolean version.
     */
    def evalTo (term : String, result : Exp) : Boolean = {
        val in = new CharArrayReader (term.toArray)
        parse (in) match {
            case Success (e, in) if in.atEnd =>
                normal (e) match {
                    case Some (r) => r == result
                    case None     => false
                }
            case _ =>
                false
        }
    }

    /**
     * Expressions with no sub-structure evaluate to themselves.
     */
    def testLeaves () {
        check ((i : Int) => (i >= 0) ==> evalTo (i.toString, Num (i)))
        check ((v : Var) => evalTo (v.toString, v))
    }

    /**
     * Tests only requiring a small number of reductions.
     */
    def testSimple () {
        assertEval ("""(\x.x) 42""", Num (42))
        assertEval ("""(\x.99) 42""", Num (99))
        assertEval ("""(\x.x) (\y.y)""", Lam ("y", Var ("y")))
        assertEval ("""(\x.99) (\y.y)""", Num (99))
    }

    /**
     * Relatively simple test from Rose's notes.
     */
    def testRose () = {
        assertEval ("""(\y.(\z.z) y) x""", Var ("x"))
    }

    /**
     * Test using encoding of Booleans and conditional.
     * 	true = \x.\y.x
     *	false = \x.\y.y
     *  if-then-else = \a.\b.\c.((a)b)c
     *  (((if-then-else)false)42)99 -> 99
     */
    def testBoolean () {
        assertEval ("""(((\a.\b.\c.((a)b)c) (\x.\y.y)) 42) 99""", Num (99))
    }

}
