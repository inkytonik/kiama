/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.bitbucket.inkytonik.kiama
package example.imperative

import ImperativeTree.Stmt
import org.bitbucket.inkytonik.kiama.util.ParsingREPL

/**
 * A read-eval-print loop for parsing imperative programs and printing thei
 * abstract synax trees.
 */
object Imperative extends ParsingREPL[Stmt] {

    import org.bitbucket.inkytonik.kiama.util.{REPLConfig, Source}
    import PrettyPrinter.format

    val banner = "Enter imperative language programs for parsing."

    override val prompt = "imperative> "

    val parsers = new SyntaxAnalyser (positions)
    val parser = parsers.stmt

    def process (source : Source, s : Stmt, config : REPLConfig) {
        config.output.emitln (s)
        config.output.emitln (format (s))
    }

}
