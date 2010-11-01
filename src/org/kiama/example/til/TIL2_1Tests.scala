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
package example.til

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TIL2_1Tests extends FunSuite {

    import AST._
    import TIL2_1Main._

    private val x = Id ("x")
    private val y = Id ("y")
    private val n = Id ("n")

    test ("transform a single for loop") {
        val input = "for x := 1 to n do write x; end"
        val tree =
            Program (List (
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x))))))
        runtest (input, tree)
    }

    test ("transform a for loop that occurs first in a sequence") {
        val input = "for x := 1 to n do write x; end write x;"
        val tree =
            Program (List (
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x)))),
                Write (Var (x))))
        runtest (input, tree)
    }

    test ("transform a for loop that occurs last in a sequence") {
        val input = "write x; for x := 1 to n do write x; end"
        val tree =
            Program (List (
                Write (Var (x)),
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x))))))
        runtest (input, tree)
    }

    test ("transform a for loop that occurs in the middle of a sequence") {
        val input = "write x; for x := 1 to n do write x; end write x;"
        val tree =
            Program (List (
                Write (Var (x)),
                Decl (x),
                For (x, Num (1), Var (n), List (
                    Write (Var (x)))),
                Write (Var (x))))
        runtest (input, tree)
    }

    test ("transform nested for loops") {
        val input = "for x := 1 to n do for y := 0 to x do write y; end end"
        val tree =
            Program (List (
                Decl (x),
                For (x, Num (1), Var (n), List (
                     Decl (y),
                     For (y, Num (0), Var (x), List (
                         Write (Var (y))))))))
        runtest (input, tree)
    }

}
