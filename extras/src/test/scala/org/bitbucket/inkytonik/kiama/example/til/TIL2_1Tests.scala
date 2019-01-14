/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.til

import org.bitbucket.inkytonik.kiama.util.TransformerTests

class TIL2_1Tests extends TransformerTests {

    import TILTree._

    val til2_1 = new TIL2_1

    def parse = til2_1.parse _

    import til2_1.transform

    val x = Id("x")
    val y = Id("y")
    val n = Id("n")

    test("transform a single for loop") {
        "for x := 1 to n do write x; end" should transformTo(
            parse, transform,
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
            parse, transform,
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
            parse, transform,
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
            parse, transform,
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
            parse, transform,
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
