/**
 * Obr language implementation tests.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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

package org.kiama.example.obr.tests

import org.kiama.example.obr.{Driver,ParserDriver,SemanticDriver}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Obr tests: compilation to assembly.
 */
@RunWith(classOf[JUnitRunner])
class ObrRegressionTests extends Driver {

    filetests ("ObrRegression", "src/org/kiama/example/obr/tests/generic", ".obr", ".risc")

}

/**
 * Obr tests: compilation and execution.
 */
@RunWith(classOf[JUnitRunner])
class ObrExecTests extends Driver {

    override val execFlag : Boolean = true

    filetests ("ObrExec", "src/org/kiama/example/obr/tests/generic", ".obr", ".out", Some (".in"), "0")

}

/**
 * Obr parser tests.
 */
@RunWith(classOf[JUnitRunner])
class ObrParserTests extends ParserDriver {

    filetests ("ObrParserEnum", "src/org/kiama/example/obr/tests/enum/parser", ".obr", ".out")
    filetests ("ObrParserException", "src/org/kiama/example/obr/tests/exceptions/parser", ".obr", ".out")

}

/**
 * Obr semantic analysis tests.
 */
@RunWith(classOf[JUnitRunner])
class ObrSemanticTests extends SemanticDriver {

    filetests ("ObrSemanticEnum", "src/org/kiama/example/obr/tests/enum/semantic", ".obr", ".out")
    filetests ("ObrSemanticException", "src/org/kiama/example/obr/tests/exceptions/semantic", ".obr", ".out")

}
