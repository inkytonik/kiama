/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2012 Anthony M Sloane, Macquarie University.
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

package org.kiama
package example.imperative

import org.kiama.util.GeneratingREPL
import org.kiama.util.ParsingREPL

/**
 * Basis for tests using the imperative language.  Includes support for generating
 * random AST instances plus convenient access to the parser and pretty-printer.
 */
trait TestBase extends Generator with Parser

/**
 * A read-eval-print loop for parsing imperative programs and printing thei
 * abstract synax trees.
 */
object Imperative extends ParsingREPL[AST.Stmt] with Parser {

    override def setup (args : Array[String]) : Boolean = {
        println ("Enter imperative language programs for parsing.")
        true
    }

    override def prompt () = "imperative> "

    def process (s : AST.Stmt) {
        println (s)
        println (PrettyPrinter.pretty (s))
    }

}

/**
 * A read-eval-print loop for generating random imperative statements.
 */
object ImperativeGen extends GeneratingREPL[AST.Stmt] with Generator {

    def generator () = arbStmt

    override def process (s : AST.Stmt) {
        println (s)
        println (PrettyPrinter.pretty (s))
    }

}
