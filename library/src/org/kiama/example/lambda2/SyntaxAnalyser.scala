/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
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

import org.kiama.util.PositionedParserUtilities

/**
 * Parser to abstract syntax for optionally typed lambda calculus.
 */
trait SyntaxAnalyser extends PositionedParserUtilities {

    import LambdaTree._

    lazy val parser =
        exp

    lazy val exp : PackratParser[Exp] =
        "\\" ~> idn ~ itype ~ ("." ~> exp) ^^ Lam |
        exp2

    lazy val itype : PackratParser[Type] =
        ":" ~> ttype |
        "" ^^ (_ => NoType ())

    lazy val exp2 : PackratParser[Exp] =
        exp2 ~ op ~ exp1 ^^ Opn |
        exp1

    lazy val exp1 : PackratParser[Exp] =
        exp1 ~ exp0 ^^ App |
        exp0

    lazy val exp0 : PackratParser[Exp] =
        number | idn ^^ Var | "(" ~> exp <~ ")"

    lazy val ttype : PackratParser[Type] =
        ttype0 ~ ("->" ~> ttype) ^^ FunType |
        ttype0

    lazy val ttype0 : PackratParser[Type] =
        "Int" ^^ (_ => IntType ()) |
        "(" ~> ttype <~ ")"

    lazy val op =
        "+" ^^ (_ => AddOp ()) |
        "-" ^^ (_ => SubOp ())

    lazy val idn =
        "[a-zA-Z][a-zA-Z0-9]*".r

    lazy val number =
        "[0-9]+".r ^^ (s => Num (s.toInt))

}
