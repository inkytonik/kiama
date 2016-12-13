/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2016 Anthony M Sloane, Macquarie University.
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
 * Support for memoisation, encapsulating common behaviour of memoised
 * entities and a general reset mechanism for all such entities.
 */
object Memoiser {

    import com.google.common.cache.{Cache, CacheBuilder}
    import scala.collection.JavaConverters._

    /**
     * Common interface for encapsulation of memoisation for a single memoised
     * entity backed by a configurable cache.
     */
    trait MemoisedBase[T, U] {

        /**
         * The memo table.
         */
        def cache : Cache[AnyRef, AnyRef]

        /**
         * Duplicate an entry if possible. If `t1` has a memoised value associated
         * with it, set the value associated with `t2` to the same value. If there
         * is no value associated with `t1`, do nothing.
         */
        def dup(t1 : T, t2 : T) {
            val u = cache.getIfPresent(t1).asInstanceOf[U]
            if (u != null)
                put(t2, u)
        }

        /**
         * Return the value stored at key `t` as an option.
         */
        def get(t : T) : Option[U] =
            Option(cache.getIfPresent(t).asInstanceOf[U])

        /**
         * Return the value stored at key `t` if there is one, otherwise
         * return `u`. `u` is only evaluated if necessary.
         */
        def getWithDefault(t : T, u : => U) : U =
            get(t).getOrElse(u)

        /**
         * Has the value at `t` already been computed or not? By default, does
         * the memo table contain a value for `t`?
         */
        def hasBeenComputedAt(t : T) : Boolean =
            get(t) != None

        /**
         * A view of the set of keys that are currently in this memo table.
         */
        def keys : Vector[T] =
            cache.asMap.keySet.asScala.toVector.asInstanceOf[Vector[T]]

        /**
         * Store the value `u` under the key `t`.
         */
        def put(t : T, u : U) {
            cache.put(t.asInstanceOf[AnyRef], u.asInstanceOf[AnyRef])
        }

        /**
         * Store the value `u` under the key `t` if `t` does not already have an
         * associated value. `u` is only evaluated if necessary.
         */
        def putIfNotPresent(t : T, u : => U) {
            if (!hasBeenComputedAt(t))
                put(t, u)
        }

        /**
         * Immediately reset the memo table.
         */
        def reset() {
            cache.invalidateAll()
        }

        /**
         * Immediately reset the memo table at `t`.
         */
        def resetAt(t : T) {
            cache.invalidate(t)
        }

        /**
         * The number of entries in the memo table.
         */
        def size() : Long =
            cache.size

        /**
         * Update the value associated with `t` by applying `f` to it. If there
         * is no value currently associated with `t`, associate it with `u`. `u`
         * is only evaluated if necessary.
         */
        def updateAt(t : T, f : U => U, u : => U) {
            get(t) match {
                case Some(v) =>
                    put(t, f(v))
                case None =>
                    put(t, u)
            }
        }

        /**
         * A view of the set of values that are currently in this memo table.
         */
        def values : Vector[U] =
            cache.asMap.values.asScala.toVector.asInstanceOf[Vector[U]]

    }

    /**
     * A memoised entity that uses equality to compare keys.
     */
    trait Memoised[T, U] extends MemoisedBase[T, U] {

        val cache : Cache[AnyRef, AnyRef] =
            CacheBuilder.newBuilder.build()

    }

    /**
     * A memoised entity that weakly holds onto its keys and uses identity
     * to compare them.
     */
    trait IdMemoised[T, U] extends MemoisedBase[T, U] {

        val cache : Cache[AnyRef, AnyRef] =
            CacheBuilder.newBuilder.weakKeys.build()

    }

}
