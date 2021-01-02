/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.transform

import TransformTree.{Program, TransformNode}
import org.bitbucket.inkytonik.kiama.util.TestCompiler

/**
 * Transformation compiler tests.
 */
class TransformTests extends Driver with TestCompiler[TransformNode, Program] {

    filetests("Transform", "example/transform/tests", ".exp", ".out")

}
