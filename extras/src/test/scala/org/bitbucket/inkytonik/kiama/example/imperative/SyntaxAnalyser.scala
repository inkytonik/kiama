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

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parser to abstract syntax tree for the imperative language.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import ImperativeTree._

    lazy val stmt : Parser[Stmt] =
        ";" ^^ (_ => Null()) | sequence | asgnStmt | whileStmt

    lazy val asgnStmt =
        variable ~ ("=" ~> exp) <~ ";" ^^ Asgn.apply

    lazy val whileStmt =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ While.apply

    lazy val sequence =
        "{" ~> rep(stmt) <~ "}" ^^ Seqn.apply

    lazy val exp : PackratParser[Exp] =
        exp ~ ("+" ~> term) ^^ Add.apply |
            exp ~ ("-" ~> term) ^^ Sub.apply |
            term

    lazy val term : PackratParser[Exp] =
        term ~ ("*" ~> factor) ^^ Mul.apply |
            term ~ ("/" ~> factor) ^^ Div.apply |
            factor

    lazy val factor : Parser[Exp] =
        double | integer | variable | "-" ~> exp ^^ Neg.apply | "(" ~> exp <~ ")"

    lazy val double =
        """[0-9]+\.[0-9]+""".r ^^ (s => Num(s.toDouble))

    lazy val integer =
        "[0-9]+".r ^^ (s => Num(s.toInt))

    lazy val variable =
        idn ^^ Var.apply

    lazy val idn =
        not(keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r

    lazy val keyword =
        keywords("[^a-zA-Z0-9]".r, List("while"))

}
