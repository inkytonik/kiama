/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
