/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2017 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.til

import org.bitbucket.inkytonik.kiama.util.TransformerTests

class TIL2_1Tests extends TransformerTests {

    import TILTree._

    val til2_1 = new TIL2_1
    val parsers = til2_1.parsers
    import parsers.program
    import til2_1.transform

    val x = Id("x")
    val y = Id("y")
    val n = Id("n")

    test("transform a single for loop") {
        "for x := 1 to n do write x; end" should transformTo(
            program, transform,
            Program(List(
                Decl(x),
                For(x, Num(1), Var(n), List(
                    Write(Var(x))
                ))
            ))
        )
    }

    test("transform a for loop that occurs first in a sequence") {
        "for x := 1 to n do write x; end write x;" should transformTo(
            program, transform,
            Program(List(
                Decl(x),
                For(x, Num(1), Var(n), List(
                    Write(Var(x))
                )),
                Write(Var(x))
            ))
        )
    }

    test("transform a for loop that occurs last in a sequence") {
        "write x; for x := 1 to n do write x; end" should transformTo(
            program, transform,
            Program(List(
                Write(Var(x)),
                Decl(x),
                For(x, Num(1), Var(n), List(
                    Write(Var(x))
                ))
            ))
        )
    }

    test("transform a for loop that occurs in the middle of a sequence") {
        "write x; for x := 1 to n do write x; end write x;" should transformTo(
            program, transform,
            Program(List(
                Write(Var(x)),
                Decl(x),
                For(x, Num(1), Var(n), List(
                    Write(Var(x))
                )),
                Write(Var(x))
            ))
        )
    }

    test("transform nested for loops") {
        "for x := 1 to n do for y := 0 to x do write y; end end" should transformTo(
            program, transform,
            Program(List(
                Decl(x),
                For(x, Num(1), Var(n), List(
                    Decl(y),
                    For(y, Num(0), Var(x), List(
                        Write(Var(y))
                    ))
                ))
            ))
        )
    }

}
