/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2019 Anthony M Sloane, Macquarie University.
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
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.LinkValue

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
                LinkValue(program, Range(0, 122)),
                LinkValue(nothing, Range(112, 121)),
                LinkValue(mainclass, Range(13, 107)),
                LinkValue(mainmethod, Range(55, 106)),
                LinkValue(println, Range(66, 105)),
                LinkValue(starexp, Range(74, 104)),
                LinkValue(otherfive, Range(93, 103)),
                LinkValue(5, Range(100, 102)),
                LinkValue(five, Range(82, 92)),
                LinkValue(5, Range(89, 91)),
                LinkValue(mul, Range(32, 46)),
                LinkValue("Mul", Range(39, 45))
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
                LinkValue(program, Range(0, 91)),
                LinkValue(mainclass, Range(0, 91)),
                LinkValue(mainmethod, Range(16, 87)),
                LinkValue(println, Range(53, 81)),
                LinkValue(starexp, Range(73, 79)),
                LinkValue(starexp, Range(73, 79)),
                LinkValue(otherfive, Range(77, 79)),
                LinkValue(five, Range(73, 75)),
                LinkValue(mul, Range(6, 10))
            )

        pretty(toDoc(program)) should produceLinks(links)

    }

}
