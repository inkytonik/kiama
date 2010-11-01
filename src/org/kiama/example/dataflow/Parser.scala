/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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
package example.dataflow

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Syntax analyser for simple imperative dataflow language.
 */
trait SyntaxAnalyser extends RegexParsers with PackratParsers {

    import DataflowAST._

    lazy val parser : PackratParser[Stm] =
        phrase (stm)

    lazy val stm : PackratParser[Stm] =
        asgnStm | whileStm | ifStm | blockStm | returnStm

    lazy val asgnStm : PackratParser[Assign] =
        idn ~ ("=" ~> exp) ^^
            { case l ~ r => Assign (l, r) }

    lazy val whileStm : PackratParser[While] =
        ("while" ~> "(" ~> exp <~ ")") ~ stm ^^
            { case e ~ b => While (e, b) }

    lazy val ifStm : PackratParser[If] =
        ("if" ~> "(" ~> exp <~ ")") ~ stm ~ ("else" ~> stm) ^^
            { case e ~ t ~ f => If (e, t, f) }

    lazy val blockStm : PackratParser[Block] =
        "{" ~> (stm*) <~ "}" ^^ Block

    lazy val returnStm : PackratParser[Return] =
        "return" ~> exp ^^ Return

    lazy val exp : PackratParser[Var] =
        idn

    lazy val idn : PackratParser[Var] =
        not (keyword) ~> "[a-zA-Z]+".r

    lazy val keyword : PackratParser[String] =
        "if" | "return" | "while"

}
