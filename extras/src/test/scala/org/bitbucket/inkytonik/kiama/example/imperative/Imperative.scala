/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.imperative

import ImperativeTree.Stmt
import org.bitbucket.inkytonik.kiama.util.ParsingREPL

/**
 * A read-eval-print loop for parsing imperative programs and printing their
 * abstract syntax trees.
 */
class ImperativeDriver extends ParsingREPL[Stmt] {

    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.{REPLConfig, Source}
    import PrettyPrinter.format

    val banner = "Enter imperative language programs for parsing."

    override val prompt = "imperative> "

    def parse(source : Source) : ParseResult[Stmt] = {
        val parsers = new SyntaxAnalyser(positions)
        parsers.parseAll(parsers.stmt, source)
    }

    def process(source : Source, s : Stmt, config : REPLConfig) : Unit = {
        config.output().emitln(s)
        config.output().emitln(format(s).layout)
    }

}

/**
 * Main object for Imperative REPL.
 */
object Imperative extends ImperativeDriver
