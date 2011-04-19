/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2011 Anthony M Sloane, Macquarie University.
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

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Parser to AST.
 */
trait Parser extends RegexParsers with PackratParsers {

    import AST._

    lazy val start : PackratParser[Stmt] =
        phrase (stmt)

    lazy val stmt : PackratParser[Stmt] =
        ";" ^^^ Null () | sequence | asgnStmt | whileStmt

    lazy val asgnStmt : PackratParser[Asgn] =
        variable ~ ("=" ~> exp) <~ ";" ^^ { case v ~ e => Asgn (v, e) }

    lazy val whileStmt : PackratParser[While] =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ { case e ~ b => While (e, b) }

    lazy val sequence : PackratParser[Seqn] =
        "{" ~> (stmt*) <~ "}" ^^ Seqn

    lazy val exp : PackratParser[Exp] =
        exp ~ ("+" ~> term) ^^ { case l ~ r => Add (l, r) } |
        exp ~ ("-" ~> term) ^^ { case l ~ r => Sub (l, r) } |
        term

    lazy val term : PackratParser[Exp] =
        term ~ ("*" ~> factor) ^^ { case l ~ r => Mul (l, r) } |
        term ~ ("/" ~> factor) ^^ { case l ~ r => Div (l, r) } |
        factor

    lazy val factor : PackratParser[Exp] =
        double | integer | variable | "-" ~> exp ^^ Neg | "(" ~> exp <~ ")"

    lazy val double : PackratParser[Num] =
        """[0-9]+\.[0-9]+""" ^^ (s => Num (s.toDouble))

    lazy val integer : PackratParser[Num] =
        "[0-9]+".r ^^ (s => Num (s.toInt))

    lazy val variable : PackratParser[Var] =
        idn ^^ Var

    lazy val idn : PackratParser[String] =
        not (keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r

    lazy val keyword : Parser[String] =
        "while"

}
