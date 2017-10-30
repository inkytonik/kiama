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
package output

import org.bitbucket.inkytonik.kiama.util.KiamaTests

/**
 * Basic tests of filters.
 */
class FiltersTests extends KiamaTests with PrettyPrinter {

    import Filters.{
        keepMaxChars,
        keepMaxIndent,
        keepMaxLines,
        keepMaxWords
    }
    import org.scalacheck.Prop._

    test("keepMaxChars can handle empty string") {
        check((i : Int) => (i >= 0) ==> (keepMaxChars(i)("") == ""))
    }

    test("keepMaxLines can handle empty string") {
        check((i : Int) => (i >= 0) ==> (keepMaxLines(i)("") == ""))
    }

    test("keepMaxWords can handle empty string") {
        check((i : Int) => (i >= 0) ==> (keepMaxWords(i)("") == ""))
    }

    {
        val output =
            """The first line
            |  the second, line
            |    the third line!
            |:the final line
            |""".stripMargin

        test("keepMaxChars can handle zero count") {
            keepMaxChars(0)(output) shouldBe ""
        }

        test("keepMaxChars can handle count in first line") {
            keepMaxChars(8)(output) shouldBe "The firs"
        }

        test("keepMaxChars can handle count in an inner line") {
            keepMaxChars(22)(output) shouldBe "The first line\n  the s"
        }

        test("keepMaxChars can handle count after end") {
            keepMaxChars(200)(output) shouldBe output
        }

        test("keepMaxLines can handle zero count") {
            keepMaxLines(0)(output) shouldBe ""
        }

        test("keepMaxLines can handle one count") {
            keepMaxLines(1)(output) shouldBe "The first line\n"
        }

        test("keepMaxLines can handle an inner count") {
            keepMaxLines(3)(output) shouldBe "The first line\n  the second, line\n    the third line!\n"
        }

        test("keepMaxLines can handle count after end") {
            keepMaxLines(10)(output) shouldBe output
        }

        test("keepMaxWords can handle zero count") {
            keepMaxWords(0)(output) shouldBe ""
        }

        test("keepMaxWords can handle count in first line") {
            keepMaxWords(2)(output) shouldBe "The first"
        }

        test("keepMaxWords can handle count in an inner line") {
            keepMaxWords(8)(output) shouldBe "The first line\n  the second, line\n    the third"
        }

        test("keepMaxWords can handle count after end") {
            keepMaxWords(20)(output) shouldBe output.init
        }

    }

    {
        val output =
            """  The first line
            |the second line
            |  the third line
            |  the fourth line
            |    the fifth line
            |    the sixth line
            |  the seventh line
            |  the eighth line
            |the ninth line
            |    the tenth line
            |""".stripMargin

        test("keepMaxIndent with indent of zero replaces all") {
            keepMaxIndent(0, output) shouldBe "...\n"
        }

        test("keepMaxIndent with indent of one keeps just top-level lines") {
            keepMaxIndent(1, output) shouldBe
                """ ...
                |the second line
                | ...
                |the ninth line
                | ...
                |""".stripMargin
        }

        test("keepMaxIndent with indent of two keeps just top-level lines") {
            keepMaxIndent(2, output) shouldBe
                """  ...
                |the second line
                |  ...
                |the ninth line
                |  ...
                |""".stripMargin
        }

        test("keepMaxIndent with indent of two and FIXME insertion works") {

            def fixmeInsertion(n : Int, s : String) : String =
                s"""${"FIXME" * n}\n"""

            keepMaxIndent(2, output, fixmeInsertion) shouldBe
                """FIXMEFIXME
                |the second line
                |FIXMEFIXME
                |the ninth line
                |FIXMEFIXME
                |""".stripMargin

        }

        test("keepMaxIndent with indent of three keeps top two levels") {
            keepMaxIndent(3, output) shouldBe
                """  The first line
                |the second line
                |  the third line
                |  the fourth line
                |   ...
                |  the seventh line
                |  the eighth line
                |the ninth line
                |   ...
                |""".stripMargin
        }

        test("keepMaxIndent with indent of four keeps top two levels") {
            keepMaxIndent(4, output) shouldBe
                """  The first line
                |the second line
                |  the third line
                |  the fourth line
                |    ...
                |  the seventh line
                |  the eighth line
                |the ninth line
                |    ...
                |""".stripMargin
        }

        test("keepMaxIndent with indent of five keeps everything") {
            keepMaxIndent(5, output) shouldBe output
        }

    }

}
