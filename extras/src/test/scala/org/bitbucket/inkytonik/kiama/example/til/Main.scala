/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.til

import TILTree.Program
import org.bitbucket.inkytonik.kiama.util.{Compiler, Config, Source}

/**
 * Main program for TIL chairmarks that just parse and print their ASTs
 * to standard output.
 */
trait ParsingMain extends Compiler[Program] {

    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}

    def process(source : Source, ast : Program, config : Config) {
        config.output().emitln(ast)
    }

    def format(ast : Program) : Document =
        emptyDocument

}

/**
 * Standard main program for TIL chairmarks that parse and transform.
 */
trait TransformingMain extends ParsingMain {

    def transform(ast : Program) : Program

    override def process(source : Source, ast : Program, config : Config) {
        val newast = transform(ast)
        super.process(source, newast, config)
    }

}
