/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.minijava

import MiniJavaTree.Program
import org.bitbucket.inkytonik.kiama.util.TestCompiler

/**
 * Tests that check that the code generator produces the expected byte code.
 */
class CodeGeneratorTests extends Driver with TestCompiler[Program] {

    val path = "example/minijava/tests"
    filetests("minijava code generation", path, ".mj", ".out")

}
