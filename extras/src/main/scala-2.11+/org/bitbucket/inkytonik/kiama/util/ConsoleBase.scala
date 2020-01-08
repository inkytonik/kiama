/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * Compatibility layer for Console reading.
 */
class ConsoleBase {

    /**
     * Read a line after prompting with the given prompt.
     */
    def readLine(prompt : String) : String =
        scala.io.StdIn.readLine(prompt)

}
