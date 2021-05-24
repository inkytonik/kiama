/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda2

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parser to abstract syntax for optionally typed lambda calculus.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import LambdaTree._

    lazy val exp : Parser[Exp] =
        "\\" ~> idn ~ itype ~ ("." ~> exp) ^^ Lam.apply |
            exp2

    lazy val itype =
        ":" ~> ttype |
            "" ^^ (_ => NoType())

    lazy val exp2 : PackratParser[Exp] =
        exp2 ~ op ~ exp1 ^^ Opn.apply |
            exp1

    lazy val exp1 : PackratParser[Exp] =
        exp1 ~ exp0 ^^ App.apply |
            exp0

    lazy val exp0 =
        number | idn ^^ Var.apply | "(" ~> exp <~ ")"

    lazy val ttype : Parser[Type] =
        ttype0 ~ ("->" ~> ttype) ^^ FunType.apply |
            ttype0

    lazy val ttype0 : Parser[Type] =
        "Int" ^^ (_ => IntType()) |
            "(" ~> ttype <~ ")"

    lazy val op =
        "+" ^^ (_ => AddOp()) |
            "-" ^^ (_ => SubOp())

    lazy val idn =
        "[a-zA-Z][a-zA-Z0-9]*".r

    lazy val number =
        "[0-9]+".r ^^ (s => Num(s.toInt))

}
