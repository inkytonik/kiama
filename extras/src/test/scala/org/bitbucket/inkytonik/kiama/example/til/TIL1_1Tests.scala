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

import org.bitbucket.inkytonik.kiama.util.{Config, ParseTests}

class TIL1_1Tests extends ParseTests {

    import TILTree._
    import org.bitbucket.inkytonik.kiama.util.StringSource

    val config = new Config(Vector())

    val til1_1 = new TIL1_1

    def parse = til1_1.parse _

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
        parse(StringSource(input), config) should parseTo(tree)
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
        parse(StringSource(input), config) should parseTo(tree)
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
        parse(StringSource(input), config) should parseTo(tree)
    }

}
