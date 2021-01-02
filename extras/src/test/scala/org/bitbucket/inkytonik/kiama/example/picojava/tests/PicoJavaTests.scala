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
package example.picojava
package tests

import org.bitbucket.inkytonik.kiama.util.TestCompilerWithConfig
import PicoJavaTree.{PicoJavaNode, Program}

/**
 * Tests that check that the PicoJava main program produces appropriate output.
 */
class PicoJavaTests extends Driver with TestCompilerWithConfig[PicoJavaNode, Program, PicojavaConfig] {

    val path = "example/picojava/tests"
    filetests("PicoJava", path, ".pj", ".out")
    filetests("PicoJava", path, ".pj", ".obsout", argslist = List(List("--obfuscate")))

}
