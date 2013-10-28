/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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
package L2

/**
 * Parsers for L2 language.
 */
trait Parser extends L1.Parser {

    import base.source.Statement
    import L0.source.IdnExp
    import scala.collection.immutable.Seq
    import source.{Case, CaseStatement, ForStatement, MinMaxCond, ValCond}

    override def statementDef : PackratParser[Statement] =
        forStatement |
        caseStatement |
        super.statementDef

    lazy val forStatement =
        "FOR" ~> forVar ~ (":=" ~> expression) ~ ("TO" ~> expression) ~ step ~
             ("DO" ~> statementSequence <~ "END") ^^ ForStatement

    lazy val forVar =
        idnuse ^^ IdnExp

    lazy val step =
        "BY" ~> expression ^^ (e => Some (e)) |
        success (None)

    lazy val caseStatement =
        ("CASE" ~> expression <~ "OF") ~ cases ~ optelse <~ "END" ^^ CaseStatement

    lazy val cases =
        rep1sep (kase, "|") |
        failure ("clause expected")

    lazy val kase =
        conditions ~ (":" ~> statementSequence) ^^ Case

    lazy val conditions =
        rep1sep (condition, ",")

    lazy val condition =
        expression ~ (".." ~> expression) ^^ MinMaxCond |
        expression ^^ ValCond

    override def keywordStrings : Seq[String] =
        "BY" +: "CASE" +: "FOR" +: "OF" +: "STEP" +: "TO" +: super.keywordStrings

}
