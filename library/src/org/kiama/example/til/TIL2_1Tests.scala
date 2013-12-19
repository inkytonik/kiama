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
package example.til

import org.kiama.util.TransformerTests

class TIL2_1Tests extends TIL2_1 with TransformerTests {

    import TILTree._
    import scala.collection.immutable.Seq

    val x = Id ("x")
    val y = Id ("y")
    val n = Id ("n")

    test ("transform a single for loop") {
        val input = "for x := 1 to n do write x; end"
        val tree =
            Program (Seq (
                Decl (x),
                For (x, Num (1), Var (n), Seq (
                    Write (Var (x))))))
        assertTransformOk (input, parser, transform, tree)
    }

    test ("transform a for loop that occurs first in a sequence") {
        val input = "for x := 1 to n do write x; end write x;"
        val tree =
            Program (Seq (
                Decl (x),
                For (x, Num (1), Var (n), Seq (
                    Write (Var (x)))),
                Write (Var (x))))
        assertTransformOk (input, parser, transform, tree)
    }

    test ("transform a for loop that occurs last in a sequence") {
        val input = "write x; for x := 1 to n do write x; end"
        val tree =
            Program (Seq (
                Write (Var (x)),
                Decl (x),
                For (x, Num (1), Var (n), Seq (
                    Write (Var (x))))))
        assertTransformOk (input, parser, transform, tree)
    }

    test ("transform a for loop that occurs in the middle of a sequence") {
        val input = "write x; for x := 1 to n do write x; end write x;"
        val tree =
            Program (Seq (
                Write (Var (x)),
                Decl (x),
                For (x, Num (1), Var (n), Seq (
                    Write (Var (x)))),
                Write (Var (x))))
        assertTransformOk (input, parser, transform, tree)
    }

    test ("transform nested for loops") {
        val input = "for x := 1 to n do for y := 0 to x do write y; end end"
        val tree =
            Program (Seq (
                Decl (x),
                For (x, Num (1), Var (n), Seq (
                     Decl (y),
                     For (y, Num (0), Var (x), Seq (
                         Write (Var (y))))))))
        assertTransformOk (input, parser, transform, tree)
    }

}
