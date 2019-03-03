/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.imperative

import ImperativeTree._
import org.bitbucket.inkytonik.kiama.util.GeneratingREPL

/**
 * A read-eval-print loop for generating random imperative statements.
 */
object ImperativeGen extends GeneratingREPL[Stmt] with Generator {

    import org.bitbucket.inkytonik.kiama.util.{REPLConfig, Source}
    import org.scalacheck.Arbitrary
    import PrettyPrinter.format

    def generator : Arbitrary[Stmt] =
        arbStmt

    override def process(source : Source, s : Stmt, config : REPLConfig) {
        super.process(source, s, config)
        config.output().emitln(format(s).layout)
    }

}
