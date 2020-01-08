/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.repmin

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.util.KiamaTests

class RepminTests extends KiamaTests {

    val t = Fork(Leaf(3), Fork(Leaf(1), Leaf(10)))
    val u = Fork(Leaf(1), Fork(Leaf(1), Leaf(1)))

    test("repmin actually reps and mins (traditional)") {
        val tree = new Tree[RepminTree, RepminTree](t)
        val repmin = new Repmin(tree)
        repmin.repmin(t) shouldBe u
    }

    test("repmin actually reps and mins (decorator)") {
        val tree = new Tree[RepminTree, RepminTree](t)
        val repmin = new RepminDec(tree)
        repmin.repmin(t) shouldBe u
    }

}
