/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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

import org.bitbucket.inkytonik.kiama.util.ParseTests

class TIL1_1Tests extends ParseTests {

    import TILTree._

    val til1_1 = new TIL1_1
    val parsers = til1_1.parsers
    import parsers.program

    val n = Id("n")
    val f = Id("f")

    test("parse factorial program") {
        val input = """
var n;
read n;
var x;
var fact;
fact := 1;
for x := 1 to n do
    fact := x * fact;
end
write "factorial of ";
write n;
write " is ";
write fact;
write "\n";"""
        val x = Id("x")
        val fact = Id("fact")
        val tree =
            Program(
                List(
                    Decl(n),
                    Read(n),
                    Decl(x),
                    Decl(fact),
                    Assign(fact, Num(1)),
                    For(x, Num(1), Var(n),
                        List(
                            Assign(fact, Mul(Var(x), Var(fact)))
                        )),
                    Write(Str("\"factorial of \"")),
                    Write(Var(n)),
                    Write(Str("\" is \"")),
                    Write(Var(fact)),
                    Write(Str("\"\\n\""))
                )
            )
        assertParseOk(input, program, tree)
    }

    test("parse factors program") {
        val input = """
var n;
write "Input n please";
read n;
write "The factors of n are";
var f;
f := 2;
while n != 1 do
    while (n / f) * f = n do
        write f;
        n := n / f;
    end
    f := f + 1;
end"""
        val tree =
            Program(
                List(
                    Decl(n),
                    Write(Str("\"Input n please\"")),
                    Read(n),
                    Write(Str("\"The factors of n are\"")),
                    Decl(f),
                    Assign(f, Num(2)),
                    While(
                        Ne(Var(n), Num(1)),
                        List(
                            While(
                                Eq(Mul(Div(Var(n), Var(f)), Var(f)), Var(n)),
                                List(
                                    Write(Var(f)),
                                    Assign(n, Div(Var(n), Var(f)))
                                )
                            ),
                            Assign(f, Add(Var(f), Num(1)))
                        )
                    )
                )
            )
        assertParseOk(input, program, tree)
    }

    test("parse multiples program") {
        val input = """
for i := 1 to 9 do
    for j := 1 to 10 do
        write i*j;
    end
end
"""
        val i = Id("i")
        val j = Id("j")
        val tree =
            Program(
                List(
                    For(i, Num(1), Num(9),
                        List(
                            For(j, Num(1), Num(10),
                                List(Write(Mul(Var(i), Var(j)))))
                        ))
                )
            )
        assertParseOk(input, program, tree)
    }

}
