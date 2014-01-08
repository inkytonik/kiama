/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package example.oberon0
package L1

/**
 * Parsers for L1 language.
 */
trait SyntaxAnalyser extends L0.SyntaxAnalyser {

    import base.source.{Block, Statement}
    import L0.source.Expression
    import scala.collection.immutable.Seq
    import source.{IfStatement, WhileStatement}

    override def statementDef : PackratParser[Statement]=
        ifStatement |
        whileStatement |
        super.statementDef

    lazy val ifStatement =
        "IF" ~> expression ~ ("THEN" ~> statementSequence) ~
            elsifs ~ (optelse <~ "END") ^^ IfStatement

    lazy val elsifs =
        rep (elsif)

    lazy val elsif : PackratParser[(Expression, Block)] =
        ("ELSIF" ~> expression) ~ ("THEN" ~> statementSequence)

    lazy val optelse =
        "ELSE" ~> statementSequence ^^ (ss => Some (ss)) |
        success (None)

    lazy val whileStatement =
        "WHILE" ~> expression ~ ("DO" ~> statementSequence <~ "END") ^^
        WhileStatement

    override def keywordStrings : Seq[String] =
        "DO" +: "ELSE" +: "ELSIF" +: "IF" +: "THEN" +: "WHILE" +: super.keywordStrings

}
