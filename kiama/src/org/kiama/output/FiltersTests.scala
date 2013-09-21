/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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
package output

import org.kiama.util.Tests
import org.scalatest.prop.Checkers

/**
 * Basic tests of filters.
 */
class FiltersTests extends Tests with Checkers with PrettyPrinter {

    import Filters.{keepMaxChars, keepMaxIndent, keepMaxLines,
        keepMaxWords}
    import org.scalacheck.Prop._

    test ("keepMaxChars can handle empty string") {
        check ((i : Int) => (i >= 0) ==> (keepMaxChars (i) ("") == ""))
    }

    test ("keepMaxLines can handle empty string") {
        check ((i : Int) => (i >= 0) ==> (keepMaxLines (i) ("") == ""))
    }

    test ("keepMaxWords can handle empty string") {
        check ((i : Int) => (i >= 0) ==> (keepMaxWords (i) ("") == ""))
    }

    {
        val output =
            """The first line
            |  the second, line
            |    the third line!
            |:the final line
            |""".stripMargin

        test ("keepMaxChars can handle zero count") {
            assertResult ("") (keepMaxChars (0) (output))
        }

        test ("keepMaxChars can handle count in first line") {
            assertResult ("The firs") (keepMaxChars (8) (output))
        }

        test ("keepMaxChars can handle count in an inner line") {
            assertResult ("The first line\n  the s") (keepMaxChars (22) (output))
        }

        test ("keepMaxChars can handle count after end") {
            assertResult (output) (keepMaxChars (200) (output))
        }

        test ("keepMaxLines can handle zero count") {
            assertResult ("") (keepMaxLines (0) (output))
        }

        test ("keepMaxLines can handle one count") {
            assertResult ("The first line\n") (keepMaxLines (1) (output))
        }

        test ("keepMaxLines can handle an inner count") {
            assertResult ("The first line\n  the second, line\n    the third line!\n") (
                keepMaxLines (3) (output)
            )
        }

        test ("keepMaxLines can handle count after end") {
            assertResult (output) (keepMaxLines (10) (output))
        }

        test ("keepMaxWords can handle zero count") {
            assertResult ("") (keepMaxWords (0) (output))
        }

        test ("keepMaxWords can handle count in first line") {
            assertResult ("The first") (keepMaxWords (2) (output))
        }

        test ("keepMaxWords can handle count in an inner line") {
            assertResult ("The first line\n  the second, line\n    the third") (
                keepMaxWords (8) (output)
            )
        }

        test ("keepMaxWords can handle count after end") {
            assertResult (output.init) (keepMaxWords (20) (output))
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

        test ("keepMaxIndent with indent of zero replaces all") {
            assertResult ("...\n") (keepMaxIndent (0, output))
        }

        test ("keepMaxIndent with indent of one keeps just top-level lines") {
            val result =
                """ ...
                |the second line
                | ...
                |the ninth line
                | ...
                |""".stripMargin
            assertResult (result) (keepMaxIndent (1, output))
        }

        test ("keepMaxIndent with indent of two keeps just top-level lines") {
            val result =
                """  ...
                |the second line
                |  ...
                |the ninth line
                |  ...
                |""".stripMargin
            assertResult (result) (keepMaxIndent (2, output))
        }

        test ("keepMaxIndent with indent of two and FIXME insertion works") {

            def fixmeInsertion (n : Int, s : String) : String =
                s"""${"FIXME" * n}\n"""

            val result =
                """FIXMEFIXME
                |the second line
                |FIXMEFIXME
                |the ninth line
                |FIXMEFIXME
                |""".stripMargin
            assertResult (result) (keepMaxIndent (2, output, fixmeInsertion))

        }

        test ("keepMaxIndent with indent of three keeps top two levels") {
            val result =
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
            assertResult (result) (keepMaxIndent (3, output))
        }

        test ("keepMaxIndent with indent of four keeps top two levels") {
            val result =
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
            assertResult (result) (keepMaxIndent (4, output))
        }

        test ("keepMaxIndent with indent of five keeps everything") {
            assertResult (output) (keepMaxIndent (5, output))
        }

    }

}
