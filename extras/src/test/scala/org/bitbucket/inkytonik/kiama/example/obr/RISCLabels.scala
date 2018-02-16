/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.obr

/**
 * Source of label numbers for transformation and encoding.
 */
class RISCLabels {

    import org.bitbucket.inkytonik.kiama.util.Counter

    /**
     * Counter for new labels.
     */
    val newLabelCounter = new Counter(0)

    /**
     * Generate a unique label number starting from one.
     */
    def genlabelnum() : Int = {
        newLabelCounter.next()
    }

    /**
     * Reset the label counter.
     */
    def reset() {
        newLabelCounter.reset()
    }

}
