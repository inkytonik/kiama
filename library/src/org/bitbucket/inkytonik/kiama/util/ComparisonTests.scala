/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2016 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package util

/**
 * Tests of comparison routiines.
 */
class ComparisonTests extends Tests {

    import Comparison.{optsame, same}

    case class Foo(i : Int)

    test("same: two equal Booleans compare equal") {
        same(false, false) shouldBe true
    }

    test("same: two unequal Booleans compare unequal") {
        same(false, true) shouldBe false
    }

    test("same: two equal integers compare equal") {
        same(1, 1) shouldBe true
    }

    test("same: two unequal integers compare unequal") {
        same(1, 2) shouldBe false
    }

    test("same: two equal longs compare equal") {
        same(1L, 1L) shouldBe true
    }

    test("same: two unequal longs compare unequal") {
        same(1L, 2L) shouldBe false
    }

    test("same: two equal floats compare equal") {
        same(4.5f, 4.5f) shouldBe true
    }

    test("same: two unequal floats compare unequal") {
        same(7.8f, 3.4f) shouldBe false
    }

    test("same: two equal doubles compare equal") {
        same(4.5, 4.5) shouldBe true
    }

    test("same: two unequal doubles compare unequal") {
        same(7.8, 3.4) shouldBe false
    }

    test("same: two equal strings compare equal") {
        same("hello", "hello") shouldBe true
    }

    test("same: two unequal strings compare unequal") {
        same("hello", "there") shouldBe false
    }

    test("same: null is not equal to a reference on left") {
        same(null, Foo(1)) shouldBe false
    }

    test("same: null is not equal to a reference on right") {
        same(Foo(1), null) shouldBe false
    }

    test("same: two unequal references compare unequal") {
        same(Foo(1), Foo(1)) shouldBe false
    }

    test("same: two equal references compare equal") {
        val r = Foo(1)
        same(r, r) shouldBe true
    }

    test("optsame: two None values compare equal") {
        optsame(None, None) shouldBe true
    }

    test("optsame: None is not equal to a Some on left") {
        optsame(None, Some(1)) shouldBe false
    }

    test("optsame: None is not equal to a Some on right") {
        optsame(Some(2), None) shouldBe false
    }

    test("optsame: Some of two unequal references compare unequal") {
        optsame(Some(Foo(1)), Some(Foo(1))) shouldBe false
    }

    test("optsame: Some of two equal references compare equal") {
        val r = Foo(1)
        optsame(Some(r), Some(r)) shouldBe true
    }

}
