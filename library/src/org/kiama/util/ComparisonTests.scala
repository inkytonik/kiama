/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2015 Anthony M Sloane, Macquarie University.
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
package util

/**
 * Tests of comparison routiines.
 */
class ComparisonTests extends Tests {

    import Comparison.{optsame, same}

    case class Foo (i : Int)

    test ("same: two equal Booleans compare equal") {
        assert (same (false, false))
    }

    test ("same: two unequal Booleans compare unequal") {
        assert (!same (false, true))
    }

    test ("same: two equal integers compare equal") {
        assert (same (1, 1))
    }

    test ("same: two unequal integers compare unequal") {
        assert (!same (1, 2))
    }

    test ("same: two equal longs compare equal") {
        assert (same (1L, 1L))
    }

    test ("same: two unequal longs compare unequal") {
        assert (!same (1L, 2L))
    }

    test ("same: two equal floats compare equal") {
        assert (same (4.5f, 4.5f))
    }

    test ("same: two unequal floats compare unequal") {
        assert (!same (7.8f, 3.4f))
    }

    test ("same: two equal doubles compare equal") {
        assert (same (4.5, 4.5))
    }

    test ("same: two unequal doubles compare unequal") {
        assert (!same (7.8, 3.4))
    }

    test ("same: two equal strings compare equal") {
        assert (same ("hello", "hello"))
    }

    test ("same: two unequal strings compare unequal") {
        assert (!same ("hello", "there"))
    }

    test ("same: null is not equal to a reference on left") {
        assert (!same (null, Foo (1)))
    }

    test ("same: null is not equal to a reference on right") {
        assert (!same (Foo (1), null))
    }

    test ("same: two unequal references compare unequal") {
        assert (!same (Foo (1), Foo (1)))
    }

    test ("same: two equal references compare equal") {
        val r = Foo (1)
        assert (same (r, r))
    }

    test ("optsame: two None values compare equal") {
        assert (optsame (None, None))
    }

    test ("optsame: None is not equal to a Some on left") {
        assert (!optsame (None, Some (1)))
    }

    test ("optsame: None is not equal to a Some on right") {
        assert (!optsame (Some (2), None))
    }

    test ("optsame: Some of two unequal references compare unequal") {
        assert (!optsame (Some (Foo (1)), Some (Foo (1))))
    }

    test ("optsame: Some of two equal references compare equal") {
        val r = Foo (1)
        assert (optsame (Some (r), Some (r)))
    }

}
