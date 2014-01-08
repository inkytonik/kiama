/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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

import com.google.common.cache.{Cache, CacheBuilder}

/**
 * Support for memoisation, encapsulating common behaviour of memoised
 * entities and a general reset mechanism for all such entities.
 */
trait Memoiser {

    /**
     * The version number of the current memo tables.
     */
    private var memoVersion = 0

    /**
     * Lazily reset all memoisation tables. The actual resets will only happen
     * the next time the value of each attribute is accessed.
     */
    def resetMemo () {
        memoVersion += 1
    }

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
         * The version number of this entities memo table.
         */
        private var thisMemoVersion = 0

        /**
         * Return the value stored at key `t` as an option.
         */
        def get (t : T) : Option[U] = {
            resetIfRequested ()
            Option (memo.getIfPresent (t).asInstanceOf[U])
        }

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
            resetIfRequested ()
            memo.put (t.asInstanceOf[AnyRef], u.asInstanceOf[AnyRef])
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

        /**
         * Check to see if a reset has been requested via the common memo
         * version, and if so, do it.
         */
        def resetIfRequested () {
            if (thisMemoVersion != memoVersion) {
                thisMemoVersion = memoVersion
                reset ()
            }
        }

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
