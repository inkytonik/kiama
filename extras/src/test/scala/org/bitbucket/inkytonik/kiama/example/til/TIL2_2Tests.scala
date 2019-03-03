/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
