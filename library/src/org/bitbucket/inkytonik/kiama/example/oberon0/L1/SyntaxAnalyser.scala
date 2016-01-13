/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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
package example.oberon0
package L1

import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parsers for L1 language.
 */
class SyntaxAnalyser(positions : Positions) extends L0.SyntaxAnalyser(positions) {

    import base.source.{Block, Expression, Statement}
    import source.{IfStatement, WhileStatement}

    override def statementDef : Parser[Statement] =
        ifStatement |
            whileStatement |
            super.statementDef

    lazy val ifStatement =
        "IF" ~> expression ~ ("THEN" ~> statementSequence) ~
            elsifs ~ (optelse <~ "END") ^^ IfStatement

    lazy val elsifs =
        rep(elsif)

    lazy val elsif : Parser[(Expression, Block)] =
        ("ELSIF" ~> expression) ~ ("THEN" ~> statementSequence)

    lazy val optelse =
        "ELSE" ~> statementSequence ^^ (ss => Some(ss)) |
            success(None)

    lazy val whileStatement =
        "WHILE" ~> expression ~ ("DO" ~> statementSequence <~ "END") ^^
            WhileStatement

    override def keywordStrings : List[String] =
        "DO" +: "ELSE" +: "ELSIF" +: "IF" +: "THEN" +: "WHILE" +: super.keywordStrings

}
