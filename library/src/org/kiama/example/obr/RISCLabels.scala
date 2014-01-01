/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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
package example.obr

/**
 * Common source of label numbers for transformation and encoding.
 */
object RISCLabels {

    import org.kiama.util.Counter

    /**
     * Counter for new labels.
     */
    val newLabelCounter = new Counter (0)

    /**
     * Generate a unique label number starting from one.
     */
    def genlabelnum () : Int = {
        newLabelCounter.next ()
    }

    /**
     * Reset the label counter.
     */
    def reset () {
        newLabelCounter.reset ()
    }

}
