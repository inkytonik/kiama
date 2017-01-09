/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
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
package example.minijava

/**
 * Tests that check that the code generator produces the expected byte code.
 */
class PrettyPrinterTests extends PrettyPrinter with org.bitbucket.inkytonik.kiama.util.PrettyPrinterTests {

    import MiniJavaTree._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Link

    val mul = IdnDef("Mul")
    val five = IntExp(5)
    val otherfive = IntExp(5)
    val starexp = StarExp(five, otherfive)
    val println = Println(starexp)
    val mainmethod = MainMethod(println)
    val mainclass = MainClass(mul, mainmethod)
    val nothing = Vector()
    val program = Program(mainclass, nothing)

    test("a simple MiniJava program pretty-prints with the correct positions using any") {

        // Program (
        //     MainClass (
        //         IdnDef ("Mul"),
        //         MainMethod (Println (StarExp (IntExp (5), IntExp (5))))),
        //     Vector ())

        val links =
            List(
                Link(program, Range(0, 131)),
                Link(nothing, Range(120, 130)),
                Link(mainclass, Range(14, 115)),
                Link(mainmethod, Range(58, 114)),
                Link(println, Range(70, 113)),
                Link(starexp, Range(79, 112)),
                Link(otherfive, Range(100, 111)),
                Link(5, Range(108, 110)),
                Link(five, Range(88, 99)),
                Link(5, Range(96, 98)),
                Link(mul, Range(34, 49)),
                Link("Mul", Range(42, 48))
            )

        pretty(any(program)) should produceLinks(links)

    }

    test("a simple MiniJava program pretty-prints with the correct positions using MiniJava pretty-printer") {

        // class Mul {
        //     public static void main () {
        //         System.out.println (5 * 5);
        //     }
        // }

        val links =
            List(
                Link(program, Range(0, 91)),
                Link(mainclass, Range(0, 91)),
                Link(mainmethod, Range(16, 87)),
                Link(println, Range(53, 81)),
                Link(starexp, Range(73, 79)),
                Link(starexp, Range(73, 79)),
                Link(otherfive, Range(77, 79)),
                Link(five, Range(73, 75)),
                Link(mul, Range(6, 10))
            )

        pretty(toDoc(program)) should produceLinks(links)

    }

}
