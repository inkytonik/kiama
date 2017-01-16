/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2017 Anthony M Sloane, Macquarie University.
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
 * A simple set that compares elements by identity.
 */
class WeakIdentityHashSet[T] {

    import org.bitbucket.inkytonik.kiama.util.Memoiser.makeIdMemoiser

    /**
     * Cache of the current set contents.
     */
    val cache = makeIdMemoiser[T, Unit]()

    /**
     * Add the value `t` to the set.
     */
    def add(t : T) {
        cache.put(t, ())
    }

    /**
     * Remove all entries from the set.
     */
    def clear() {
        cache.reset()
    }

    /**
     * Is the value `t` in the set?
     */
    def contains(t : T) : Boolean =
        cache.hasBeenComputedAt(t)

    /**
     * Remove the value `t` from the set if it is a member.
     */
    def remove(t : T) {
        cache.resetAt(t)
    }

}
