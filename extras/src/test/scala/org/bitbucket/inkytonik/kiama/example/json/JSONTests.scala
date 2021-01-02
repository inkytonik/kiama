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
package example.json

import org.bitbucket.inkytonik.kiama.util.TestCompiler
import JSONTree.{JSONNode, JValue}

/**
 * Tests that check that the JSON main program produces appropriate output.
 */
class JSONTests extends Driver with TestCompiler[JSONNode, JValue] {

    val path = "example/json/tests"
    filetests("JSON", path, ".json", ".out")

}
