/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2014 Dominic Verity, Macquarie University.
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
package example.obr.tests

import org.kiama.example.obr._
import org.kiama.example.obr.ObrTree.ObrInt
import org.kiama.util.TestCompilerWithConfig
import scala.collection.immutable.Seq

/**
 * Obr regression tests: compilation to assembly.
 */
class ObrRegressionTests extends Driver with TestCompilerWithConfig[ObrInt,ObrConfig] {

    filetests ("ObrRegression", "library/src/org/kiama/example/obr/tests/generic", ".obr", ".risc",
               argslist = Seq (Seq ("-a")))

}

/**
 * Obr parser tests.
 */
class ObrParserTests extends ParserDriver with TestCompilerWithConfig[ObrInt,ObrConfig] {

    filetests ("ObrParserEnum", "library/src/org/kiama/example/obr/tests/enum/parser", ".obr", ".out")
    filetests ("ObrParserException", "library/src/org/kiama/example/obr/tests/exceptions/parser", ".obr", ".out")

}

/**
 * Obr semantic analysis tests.
 */
class ObrSemanticTests extends SemanticDriver with TestCompilerWithConfig[ObrInt,ObrConfig] {

    filetests ("ObrSemanticEnum", "library/src/org/kiama/example/obr/tests/enum/semantic", ".obr", ".out")
    filetests ("ObrSemanticException", "library/src/org/kiama/example/obr/tests/exceptions/semantic", ".obr", ".out")

}

/**
 * Obr tests: compilation and execution.
 */
class ObrExecTests extends Driver with TestCompilerWithConfig[ObrInt,ObrConfig] {

    import org.kiama.util.{Config, StringEmitter}

    filetests ("ObrExec", "library/src/org/kiama/example/obr/tests/generic", ".obr", ".out",
               Some (".in"), "0", Seq (Seq ("-e")))

    /*
     * Method to execute an execution test on a single Obr program
     * Parameters:
     *     name         name of this test to identify it in the log output
     *     dirname      path of the directory in which the Obr test source files reside
     *     spec         a test specification which consists of a 3-tuple containing:
     *                          - the name of the Obr source file to compile and execute
     *                          - a list containing the parameters to pass to the Obr program
     *                          - the corresponding result we expect the program to produce
     */
    def exectest (name : String, dirname : String, spec : (String, Seq[Int], Int)) {
        val (obrfile, params, expect) = spec
        val title = s"""$name processing $obrfile parameters ${params.mkString("(",", ",")")} expecting $expect"""
        test (title) {
            val emitter = new StringEmitter
            val args = Seq ("--Kconsole", "string", params.mkString ("", "\n", "\n"),
                            "-e", dirname + obrfile)
            val config = createConfig (args, emitter)
            try {
                testdriver (config)
            } catch {
                case e : Exception =>
                    info ("failed with an exception ")
                    throw (e)
            }
            val output = emitter.result
            if (output != s"$expect\n")
                fail (s"$title generated bad output: $output")
        }
    }

    // Execution tests for FOR loops
    val forExecDir = "library/src/org/kiama/example/obr/tests/for/codegen/"
    val forExecTests = Seq (
            ("for.obr", Seq (0, 0), 0)
        ,   ("for.obr", Seq (5, 0), 0)
        ,   ("for.obr", Seq (-1, 3), 0)
        ,   ("for.obr", Seq (5, 5), 25)
        ,   ("for2.obr", Seq (2), 2)
        ,   ("for2.obr", Seq (0), 1)
        ,   ("for2.obr", Seq (-1), 1)
        ,   ("for2.obr", Seq (1), 1)
        ,   ("for2.obr", Seq (5), 120)
        ,   ("for3.obr", Seq (2), 2)
        ,   ("for3.obr", Seq (0), 1)
        ,   ("for3.obr", Seq (-1), 1)
        ,   ("for3.obr", Seq (1), 1)
        ,   ("for3.obr", Seq (5), 141)
    )
    forExecTests.map(exectest("ObrForExec", forExecDir, _))

    // Execution tests for code involving enumeration values.

    val enumExecDir = "library/src/org/kiama/example/obr/tests/enum/codegen/"
    val enumExecTests = Seq (
            ("enumtest.obr", Seq (-1), 0)
        ,   ("enumtest.obr", Seq (0), 1)
        ,   ("enumtest.obr", Seq (1), 1)
        ,   ("enumtest.obr", Seq (2), 1)
        ,   ("enumtest.obr", Seq (3), 1)
        ,   ("enumtest.obr", Seq (4), 0)
        ,   ("enumtest.obr", Seq (5), 0)
        ,   ("enumtest.obr", Seq (6), 0)
        ,   ("enumtest.obr", Seq (7), 0)
        )
    enumExecTests.map(exectest("ObrEnumExec", enumExecDir, _))

    // Execution tests for code involving exception handling.

    val exceptionsExecDir = "library/src/org/kiama/example/obr/tests/exceptions/codegen/"
    val exceptionsExecTests = Seq (
            ("except1a.obr", Seq (0), -1)

        ,   ("except1b.obr", Seq (0), -2)
        ,   ("except1b.obr", Seq (20), 20)

        ,   ("except1c.obr", Seq (-20), -20)
        ,   ("except1c.obr", Seq (0), -1)
        ,   ("except1c.obr", Seq (1), -2)
        ,   ("except1c.obr", Seq (2), -3)
        ,   ("except1c.obr", Seq (3), 3)

        ,   ("except2a.obr", Seq (75), -2)
        ,   ("except2a.obr", Seq (0), -1)

        ,   ("except2b.obr", Seq (3), -2)
        ,   ("except2b.obr", Seq (2), 2)
        ,   ("except2b.obr", Seq (1), 11)
        ,   ("except2b.obr", Seq (0), 110)

        ,   ("except3.obr", Seq (-32), 68)

        ,   ("except4a.obr", Seq (-3), -33)
        ,   ("except4a.obr", Seq (0), -1)
        ,   ("except4a.obr", Seq (10), 10)

        ,   ("except4b.obr", Seq (23), -1)
        ,   ("except4b.obr", Seq (16), -1)
        ,   ("except4b.obr", Seq (10),
                (100.asInstanceOf[Int] / ((10 * 10 - 39 * 10) + 368)) * 12 + 28)
        ,   ("except4b.obr", Seq (20),
                (100.asInstanceOf[Int] / ((20 * 20 - 39 * 20) + 368)) * 12 + 28)

        ,   ("except5a.obr", Seq (0), -2)
        ,   ("except5a.obr", Seq (20), 5)

        ,   ("except5b.obr", Seq (23), -2)
        ,   ("except5b.obr", Seq (16), -2)
        ,   ("except5b.obr", Seq (10),
                (100.asInstanceOf[Int] / ((10 * 10 - 39 * 10) + 368)) * 12 + 28)
        ,   ("except5b.obr", Seq (-2),
                (100.asInstanceOf[Int] / (((-2) * (-2) - 39 * (-2)) + 368)) * 12 + 28)

        ,   ("except6a.obr", Seq (0), -1)
        ,   ("except6a.obr", Seq (1), 10)
        ,   ("except6a.obr", Seq (2), 20)
        ,   ("except6a.obr", Seq (3), 30)
        ,   ("except6a.obr", Seq (4), -1)
        ,   ("except6a.obr", Seq (-10), -1000)
        ,   ("except6a.obr", Seq (10), 1000)

        ,   ("except6b.obr", Seq (0), -1)
        ,   ("except6b.obr", Seq (1), -4)
        ,   ("except6b.obr", Seq (2), -3)
        ,   ("except6b.obr", Seq (3), -2)
        ,   ("except6b.obr", Seq (4), -1)
        ,   ("except6b.obr", Seq (5), -4)
        ,   ("except6b.obr", Seq (6), -3)
        ,   ("except6b.obr", Seq (7), -1)
        ,   ("except6b.obr", Seq (8), 0)
        ,   ("except6b.obr", Seq (-152), 0)

        ,   ("except6c.obr", Seq (0), 1)
        ,   ("except6c.obr", Seq (5), 500)

        ,   ("except7a.obr", Seq (0), -2)
        ,   ("except7a.obr", Seq (23), 2300)

        ,   ("except7b.obr", Seq (0), 11)
        ,   ("except7b.obr", Seq (1), 12)
        ,   ("except7b.obr", Seq (2), 13)
        ,   ("except7b.obr", Seq (3), 14)
        ,   ("except7b.obr", Seq (4), -1)
        ,   ("except7b.obr", Seq (10), 0)
        ,   ("except7b.obr", Seq (-10), 0)

        ,   ("outofbounds.obr", Seq (0), -400)
        ,   ("outofbounds.obr", Seq (11), -1)
        ,   ("outofbounds.obr", Seq (1), -200)
        ,   ("outofbounds.obr", Seq (2), 5000)
        ,   ("outofbounds.obr", Seq (5), 500)
        ,   ("outofbounds.obr", Seq (8), 178)
        )
    exceptionsExecTests.map(exectest("ObrExceptionsExec", exceptionsExecDir, _))

}

/**
 * Obr tests: check that exception and enumeration numbers are correct.
 * Simply compiles the files except8.obr and enumtest2.obr then collects
 * together the list of integers which occur in the IntDatum leaves of the
 * resulting RISCTrees (in bottom up, left to right post-order). Having done
 * that it then checks that list to see if the integers that should be attached
 * to applied uses of each constant of the original source file occur
 * in an appropriate order.
 */
class ObrNumberingTests extends TreeTestDriver {

    targettreetest("ObrNumbering", "library/src/org/kiama/example/obr/tests/exceptions/codegen/",
                   "except8.obr", checkintdatums (Seq (3,1,4,0,2)))
    targettreetest("ObrNumbering", "library/src/org/kiama/example/obr/tests/enum/codegen/",
                   "enumtest2.obr", checkintdatums (Seq (1,3,1,0,2,2,1,0,1)))

}
