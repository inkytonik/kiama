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

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parser to abstract syntax tree for the imperative language.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import ImperativeTree._
    import scala.language.postfixOps

    lazy val stmt : Parser[Stmt] =
        ";" ^^ (_ => Null()) | sequence | asgnStmt | whileStmt

    lazy val asgnStmt =
        variable ~ ("=" ~> exp) <~ ";" ^^ Asgn

    lazy val whileStmt =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ While

    lazy val sequence =
        "{" ~> (stmt*) <~ "}" ^^ Seqn

    lazy val exp : Parser[Exp] =
        exp ~ ("+" ~> term) ^^ Add |
            exp ~ ("-" ~> term) ^^ Sub |
            term

    lazy val term : Parser[Exp] =
        term ~ ("*" ~> factor) ^^ Mul |
            term ~ ("/" ~> factor) ^^ Div |
            factor

    lazy val factor : Parser[Exp] =
        double | integer | variable | "-" ~> exp ^^ Neg | "(" ~> exp <~ ")"

    lazy val double =
        """[0-9]+\.[0-9]+""" ^^ (s => Num(s.toDouble))

    lazy val integer =
        "[0-9]+".r ^^ (s => Num(s.toInt))

    lazy val variable =
        idn ^^ Var

    lazy val idn =
        not(keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r

    lazy val keyword =
        keywords("[^a-zA-Z0-9]".r, List("while"))

}
