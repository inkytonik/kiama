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
package attribution

/**
 * Global state for the attribute memoisation tables.
 */
object MemoState {

    /**
     * The version number of the current memo tables.
     */
    private var memoVersion = 0

    /**
     * The current memo table version.
     */
    def getMemoVersion : Int =
        memoVersion

    /**
     * Lazily reset all memoisation tables. The actual resets will only happen
     * the next time the value of each attribute is accessed.
     */
    def resetMemo () {
        memoVersion += 1
    }

}

