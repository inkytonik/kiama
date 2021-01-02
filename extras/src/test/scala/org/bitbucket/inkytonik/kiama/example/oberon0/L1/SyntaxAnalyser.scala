/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
