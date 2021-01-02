/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2017-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.prolog

import org.bitbucket.inkytonik.kiama.util.TestREPLWithConfig

/**
 * Tests that check that the REPL produces appropriate output.
 */
class PrologREPLTests extends PrologDriver with TestREPLWithConfig[PrologConfig] {

    val path = "example/prolog/tests"
    filetests("Prolog REPL", path, ".repl", ".replout",
        argslist = List(List(
            "--database",
            "src/test/scala/org/bitbucket/inkytonik/kiama/example/prolog/tests/family.pl"
        )))

}
