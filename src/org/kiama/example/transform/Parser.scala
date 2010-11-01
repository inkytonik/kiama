/*
 * Parser for transformation compiler.
 *
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
package example.transform

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Parse the input.
 */
trait Parser extends RegexParsers with PackratParsers {

    import AST._

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        rep (opdecl) ~ rep (vardecl) ~ exp ^^ {
            case ops ~ vars ~ e => Program (ops, vars, e)
        }

    lazy val opdecl : PackratParser[(String,Int)] =
        ("op" ~> op) ~ integer ^^ {
            case n ~ i => (n, i)
        }

    lazy val op : PackratParser[String] =
        regex ("[-!@#$%^&*+_=:;<>,.?]+".r)

    lazy val vardecl : PackratParser[VarDecl] =
        "var" ~> ident ^^ VarDecl

    lazy val exp : PackratParser[ExpR] =
        factor ~ op ~ exp ^^ {
            case l ~ o ~ r => BinExpR (l, o, r)
        } |
        factor ^^ Factor

    lazy val factor : PackratParser[PrimExp] =
        positioned (
            integer ^^ Num |
            ident ^^ Var
        )

    lazy val integer : PackratParser[Int] =
        "[0-9]+".r ^^ (s => s.toInt)

    lazy val ident : PackratParser[String] =
        regex ("[a-zA-Z]+".r)

    override protected val whiteSpace =
        """(\s|(/\*(?:.|[\n\r])*?\*/))+""".r

}


