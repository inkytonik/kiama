/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.imperative

import org.bitbucket.inkytonik.kiama.util.TestREPL

/**
 * Tests that check that the REPL produces appropriate output.
 */
class ImperativeREPLTests extends ImperativeDriver with TestREPL {

    val path = "example/imperative/tests"
    filetests("Imperative REPL", path, ".repl", ".replout")

}
