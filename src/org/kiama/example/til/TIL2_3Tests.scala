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
class TIL2_3Tests extends FunSuite {

    import AST._
    import TIL2_3Main._

    test ("transform a program with many nested declarations") {
        val input = """
var d;
d := 17;
var r;
r := 5;
var y;
read y;
var z;
read z;
while y != 0 do
    var x;
    x := y + z;
    var a;
    a := 3;
    var j;
    j := 1;
    var b;
    while j != 100 do
        var k;
        k := a + z;
        b := j * z;
        d := (y + z) * d;
        var e;
        e := (x + z) * r;
        j := j + 1;
    end
    var c;
    c := a + y;
    var m;
    m := y * b;
    var n;
    n := r * y;
    write n;
    y := y - 1;
end
"""
        val a = Id ("a")
        val b = Id ("b")
        val c = Id ("c")
        val d = Id ("d")
        val e = Id ("e")
        val j = Id ("j")
        val k = Id ("k")
        val m = Id ("m")
        val n = Id ("n")
        val r = Id ("r")
        val x = Id ("x")
        val y = Id ("y")
        val z = Id ("z")
        val tree = Program (List (
            Decl (d),
            Decl (r),
            Decl (y),
            Decl (z),
            Decl (x),
            Decl (a),
            Decl (j),
            Decl (b),
            Decl (k),
            Decl (e),
            Decl (c),
            Decl (m),
            Decl (n),
            Assign (d, Num (17)),
            Assign (r, Num (5)),
            Read (y),
            Read (z),
            While (Ne (Var (y), Num (0)), List (
                Assign (x, Add (Var (y), Var (z))),
                Assign (a, Num (3)),
                Assign (j, Num (1)),
                While (Ne (Var (j), Num (100)), List (
                    Assign (k, Add (Var (a), Var (z))),
                    Assign (b, Mul (Var (j), Var (z))),
                    Assign (d, Mul (Add (Var (y), Var (z)), Var (d))),
                    Assign (e, Mul (Add (Var (x), Var (z)), Var (r))),
                    Assign (j, Add (Var (j), Num (1))))),
                    Assign (c, Add (Var (a), Var (y))),
                    Assign (m, Mul (Var (y), Var (b))),
                    Assign (n, Mul (Var (r), Var (y))),
                    Write (Var (n)),
                    Assign (y, Sub (Var (y), Num (1)))))))
        runtest (input, tree)
    }

}
