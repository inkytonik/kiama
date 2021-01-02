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
package example.lambda2

import org.bitbucket.inkytonik.kiama.util.TestREPLWithConfig

/**
 * Tests that check that the REPL produces appropriate output.
 */
class LambdaREPLTests extends LambdaDriver with TestREPLWithConfig[LambdaConfig] {

    val path = "example/lambda2/tests"
    filetests("Lambda2 REPL", path, ".repl", ".replout")

}
