/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2016 Anthony M Sloane, Macquarie University.
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

class TIL2_2Tests extends TransformerTests {

    import TILTree._

    val til2_2 = new TIL2_2
    val parsers = til2_2.parsers
    import parsers.program
    import til2_2.transform

    test("transform a single for loop") {
        val x = Id("x")
        val upperx = Id("Upperx")
        "for x := 1 to n do write x; end" should transformTo(
            program, transform,
            Program(List(
                Decl(x),
                Assign(x, Num(1)),
                Decl(upperx),
                Assign(upperx, Add(Var(Id("n")), Num(1))),
                While(Sub(Var(x), Var(upperx)), List(
                    Write(Var(x)),
                    Assign(x, Add(Var(x), Num(1)))
                ))
            ))
        )
    }

    test("transform nested for loops") {
        val i = Id("i")
        val upperi = Id("Upperi")
        val j = Id("j")
        val upperj = Id("Upperj")
        "for i := 1 to 9 do for j := 1 to 10 do write i*j; end end" should transformTo(
            program, transform,
            Program(List(
                Decl(i),
                Assign(i, Num(1)),
                Decl(upperi),
                Assign(upperi, Add(Num(9), Num(1))),
                While(Sub(Var(i), Var(upperi)), List(
                    Decl(j),
                    Assign(j, Num(1)),
                    Decl(upperj),
                    Assign(upperj, Add(Num(10), Num(1))),
                    While(Sub(Var(j), Var(upperj)), List(
                        Write(Mul(Var(i), Var(j))),
                        Assign(j, Add(Var(j), Num(1)))
                    )),
                    Assign(i, Add(Var(i), Num(1)))
                ))
            ))
        )
    }

}
