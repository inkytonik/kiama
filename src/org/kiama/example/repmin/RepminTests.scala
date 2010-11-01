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
package example.repmin

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait RepminTestsBase extends FunSuite {

    self : RepminImpl =>

    test ("repmin actually reps and mins") {
        val t = Fork (Leaf (3), Fork (Leaf (1), Leaf (10)))
        expect (Fork (Leaf (1), Fork (Leaf (1), Leaf (1)))) (t->repmin)
    }

}

@RunWith(classOf[JUnitRunner])
class RepminTests extends Repmin with RepminTestsBase

@RunWith(classOf[JUnitRunner])
class RepminDecTests extends RepminDec with RepminTestsBase
