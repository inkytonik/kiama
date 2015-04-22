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
 * Thread-safe counters. This class provides an operation that can be used
 * to generate a sequence of integer values. Instances of this class are
 * useful for generating things like unique names for generated entities.
 * The methods synchronize on the counter value so they can be called safely
 * from more than one thread.
 *
 * `init` is the initial value of the counter (default: -1).
 */
class Counter (init : Int = -1) {

    /**
     * The most recent value that was generated, or -1 if no values have
     * been generated.
     */
    private[this] var _value = init

    /**
     * Return the current value of the counter.
     */
    def value : Int =
        synchronized {
            _value
        }

    /**
     * Increment the stored value of the counter and return its new value.
     * `inc` is the amount to increment by (default: 1).
     */
    def next (inc : Int = 1) : Int = {
        synchronized {
            _value = _value + inc
            _value
        }
    }

    /**
     * Reset the value, by default to the initial value of the counter.
     */
    def reset (to : Int = init) {
        synchronized {
            _value = to
        }
    }

}
