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
 * A simple set that holds onto its elements weakly and compares them by
 * identity.
 */
class WeakIdentityHashSet[T] {

    import com.google.common.cache.{Cache, CacheBuilder}

    /**
     * Cache of the current set contents.
     */
    val cache : Cache[AnyRef,AnyRef] =
        CacheBuilder.newBuilder ().weakKeys ().build ()

    /**
     * Add the value `t` to the set.
     */
    def add (t : T) {
        cache.put (t.asInstanceOf[AnyRef], ().asInstanceOf[AnyRef])
    }

    /**
     * Remove all entries from the set.
     */
    def clear () {
        cache.invalidateAll ()
    }

    /**
     * Is the value `t` in the set?
     */
    def contains (t : T) : Boolean =
        cache.getIfPresent (t) != null

    /**
     * Remove the value `t` from the set if it is a member.
     */
    def remove (t : T) {
        cache.invalidate (t)
    }

}
