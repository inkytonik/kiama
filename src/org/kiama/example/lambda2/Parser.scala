/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
package example.lambda2

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Parser to AST.
 */
trait Parser extends RegexParsers with PackratParsers {

    import AST._
    import Lambda.typecheck

    lazy val start : PackratParser[Exp] =
        exp

    lazy val exp : PackratParser[Exp] =
        "\\" ~> idn ~ itype ~ ("." ~> exp) ^^
            { case i ~ t ~ e => Lam (i, t, e) } |
        exp2

    def itype : PackratParser[Type] =
        if (typecheck) (":" ~> ttype) else ("" ^^^ null)

    lazy val exp2 : PackratParser[Exp] =
        exp2 ~ op ~ exp1 ^^ { case l ~ o ~ r => Opn (o, l, r) } |
        exp1

    lazy val exp1 : PackratParser[Exp] =
        exp1 ~ exp0 ^^ { case l ~ r => App (l, r) } |
        exp0

    lazy val exp0 : PackratParser[Exp] =
        positioned (number | idn ^^ Var) |
        "(" ~> positioned (exp) <~ ")"

    lazy val ttype : PackratParser[Type] =
        ttype0 ~ ("->" ~> ttype) ^^ { case l ~ r => FunType (l, r) } |
        ttype0

    lazy val ttype0 : PackratParser[Type] =
        "Int" ^^^ IntType |
        "(" ~> ttype <~ ")"

    lazy val op : PackratParser[Op] =
        "+" ^^^ AddOp |
        "-" ^^^ SubOp

    lazy val idn : Parser[Idn] =
        "[a-zA-Z][a-zA-Z0-9]*".r

    lazy val number : PackratParser[Num] =
        "[0-9]+".r ^^ (s => Num (s.toInt))

}
