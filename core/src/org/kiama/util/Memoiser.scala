/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2015 Anthony M Sloane, Macquarie University.
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
 * Support for memoisation, encapsulating common behaviour of memoised
 * entities and a general reset mechanism for all such entities.
 */
trait Memoiser {

    import com.google.common.cache.{Cache, CacheBuilder}

    /**
     * Common interface for encapsulation of memoisation for a single memoised
     * entity backed by a configurable cache.
     */
    trait MemoisedBase[T,U] {

        /**
         * The memo table.
         */
        def memo : Cache[AnyRef,AnyRef]

        /**
         * Duplicate an entry if possible. If `t1` has a memoised value associated
         * with it, set the value associated with `t2` to the same value. If there
         * is no value associated with `t1`, set the value associated with `t2` to
         * `u`.
         */
        def dup (t1 : T, t2 : T, u : U) {
            put (t2, getWithDefault (t1, u))
        }

        /**
         * Return the value stored at key `t` as an option.
         */
        def get (t : T) : Option[U] =
            Option (memo.getIfPresent (t).asInstanceOf[U])

        /**
         * Return the value stored at key `t` if there is one, otherwise
         * return `u`.
         */
        def getWithDefault (t : T, u : U) : U =
            get (t).getOrElse (u)

        /**
         * Store the value `u` under the key `t`.
         */
        def put (t : T, u : U) {
            memo.put (t.asInstanceOf[AnyRef], u.asInstanceOf[AnyRef])
        }

        /**
         * Store the value `u` under the key `t` if `t` does not already have an
         * associated value.
         */
        def putIfNotPresent (t : T, u : U) {
            if (!hasBeenComputedAt (t))
                put (t, u)
        }

        /**
         * Immediately reset the memo table.
         */
        def reset () {
            memo.invalidateAll ()
        }

        /**
         * Immediately reset the memo table at `t`.
         */
        def resetAt (t : T) {
            memo.invalidate (t)
        }

        /**
         * Has the value of this attribute at `t` already been computed or not?
         * By default, does the memo table contain a value for `t`?
         */
        def hasBeenComputedAt (t : T) : Boolean =
            get (t) != None

    }

    /**
     * A memoised entity that uses equality to compare keys.
     */
    trait Memoised[T,U] extends MemoisedBase[T,U] {

        val memo : Cache[AnyRef,AnyRef] =
            CacheBuilder.newBuilder ().build ()

    }

    /**
     * A memoised entity that weakly holds onto its keys and uses identity
     * to compare them.
     */
    trait IdMemoised[T,U] extends MemoisedBase[T,U] {

        val memo : Cache[AnyRef,AnyRef] =
            CacheBuilder.newBuilder ().weakKeys ().build ()

    }

}
