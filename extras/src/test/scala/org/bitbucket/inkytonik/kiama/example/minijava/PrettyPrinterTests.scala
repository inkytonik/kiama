/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2018 Anthony M Sloane, Macquarie University.
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

        // Program(
        //     MainClass(
        //         IdnDef("Mul"),
        //         MainMethod(Println(StarExp(IntExp(5), IntExp(5))))),
        //     Vector())

        val links =
            List(
                Link(program, Range(0, 122)),
                Link(nothing, Range(112, 121)),
                Link(mainclass, Range(13, 107)),
                Link(mainmethod, Range(55, 106)),
                Link(println, Range(66, 105)),
                Link(starexp, Range(74, 104)),
                Link(otherfive, Range(93, 103)),
                Link(5, Range(100, 102)),
                Link(five, Range(82, 92)),
                Link(5, Range(89, 91)),
                Link(mul, Range(32, 46)),
                Link("Mul", Range(39, 45))
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
