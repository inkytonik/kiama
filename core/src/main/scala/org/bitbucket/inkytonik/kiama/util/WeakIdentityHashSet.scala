/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
