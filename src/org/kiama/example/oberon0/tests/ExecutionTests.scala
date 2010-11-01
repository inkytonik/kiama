/**
 * Oberon0 execution tests.
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

package org.kiama
package example.oberon0.tests

import org.kiama.example.oberon0.Driver
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Oberon0 execution tests.
 */
@RunWith(classOf[JUnitRunner])
class ExecutionTests extends Driver {

    filetests ("Oberon0", "src/org/kiama/example/oberon0/tests", ".ob0", ".out", Some (".in"))

}
