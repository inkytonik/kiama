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
package example.lambda

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
 * Lambda calculus tests.
 */
@RunWith(classOf[JUnitRunner])
class LambdaTests extends FunSuite with Checkers with TestBase {

    import AST._
    import org.scalacheck.Prop._

    /**
     * Parse and evaluate term then compare to result. Fail if any the parsing
     * or the comparison fail.
     */
    def expectEval (term : String, result : Exp) {
        parse (start, term) match {
            case Success (e, in) if in.atEnd =>
                normal (e) match {
                    case Some (r) => expect (result) (r)
                    case None     => fail ("reduction failed: " + term)
                }
            case Success (_, in) =>
                fail ("extraneous input at " + in.pos + ": " + term)
            case f =>
                fail ("parse failure: " + f)
        }
    }

   /**
     * Parse and evaluate term then compare to expected result. Fail if the
     * parsing or the comparison fail.
     */
    def evalTo (term : String, result : Exp) : Boolean = {
        parse (start, term) match {
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
