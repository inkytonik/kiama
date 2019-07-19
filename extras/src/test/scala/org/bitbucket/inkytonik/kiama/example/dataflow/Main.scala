/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.dataflow

import DataflowTree._
import org.bitbucket.inkytonik.kiama.util.Compiler

/**
 * Parse a simple imperative language program, calculate its dataflow
 * relations and use them to remove dead assignments.
 */
class Driver extends Compiler[Stm, Stm] {

    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.{Config, Source}

    val name = "dataflow"

    def parse(source : Source, config : Config) : ParseResult[Stm] = {
        val parsers = new SyntaxAnalyser(positions)
        parsers.parseAll(parsers.stm, source)
    }

    def process(source : Source, ast : Stm, config : Config) : Unit = {
        val tree = new DataflowTree(ast)
        val optimiser = new Optimiser(tree)
        val optast = optimiser.run(ast)
        config.output().emitln(optast)
    }

    def format(ast : Stm) : Document =
        emptyDocument

}

/**
 * Dataflow language implementation main program.
 */
object Main extends Driver
