/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L2

import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parsers for L2 language.
 */
class SyntaxAnalyser(positions : Positions) extends L1.SyntaxAnalyser(positions) {

    import base.source.Statement
    import L0.source.IdnExp
    import source.{Case, CaseStatement, ForStatement, MinMaxCond, ValCond}

    override def statementDef : Parser[Statement] =
        forStatement |
            caseStatement |
            super.statementDef

    lazy val forStatement =
        "FOR" ~> forVar ~ (":=" ~> expression) ~ ("TO" ~> expression) ~ step ~
            ("DO" ~> statementSequence <~ "END") ^^ ForStatement

    lazy val forVar =
        idnuse ^^ IdnExp

    lazy val step =
        "BY" ~> expression ^^ (e => Some(e)) |
            success(None)

    lazy val caseStatement =
        ("CASE" ~> expression <~ "OF") ~ cases ~ optelse <~ "END" ^^ CaseStatement

    lazy val cases =
        rep1sep(kase, "|") |
            failure("clause expected")

    lazy val kase =
        conditions ~ (":" ~> statementSequence) ^^ Case

    lazy val conditions =
        rep1sep(condition, ",")

    lazy val condition =
        expression ~ (".." ~> expression) ^^ MinMaxCond |
            expression ^^ ValCond

    override def keywordStrings : List[String] =
        "BY" +: "CASE" +: "FOR" +: "OF" +: "STEP" +: "TO" +: super.keywordStrings

}
