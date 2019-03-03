/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package base

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parsers for base language.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import source.{Block, Declaration, EmptyStmt, IdnDef, IdnUse, ModuleDecl, Statement}

    lazy val moduledecl =
        "MODULE" ~> (idndef <~ ";") ~ block ~ (idnuse <~ ".") ^^ ModuleDecl

    // Statement sequences produce blocks, which is not strictly necessary
    // for the source language but it makes transformation easier since
    // declarations can be added to the blocks later. At the moment there
    // will never be any declarations from parsing, but we leave this action
    // general in case that changes.

    lazy val block =
        declarations ~ statements ^^ {
            case ds1 ~ Block(ds2, ss) =>
                Block(ds1 ++ ds2, ss)
        }

    lazy val declarations =
        declarationsDef

    def declarationsDef : Parser[Vector[Declaration]] =
        "" ^^^ Vector()

    lazy val statements =
        "BEGIN" ~> statementSequence <~ "END" |
            "END" ^^ (_ => Block(Vector(), Vector()))

    lazy val statementSequence =
        rep1sep(statement, ";") ^^ {
            case ss =>
                Block(Vector(), ss)
        }

    lazy val statement =
        statementDef

    def statementDef : Parser[Statement] =
        success(EmptyStmt())

    lazy val idndef =
        ident ^^ IdnDef

    lazy val idnuse =
        ident ^^ IdnUse

    def keywordStrings : List[String] =
        List("BEGIN", "END", "MODULE")

    lazy val keyword =
        keywords("[^a-zA-Z0-9]".r, keywordStrings)

    lazy val ident =
        not(keyword) ~> "[a-zA-Z_][a-zA-Z0-9]*".r |
            failure("ident expected")

    override val whitespace =
        rep("""\s+""".r | comment)

    lazy val comment : Parser[Any] =
        "(*" ~ rep(not("*)") ~ (comment | any)) ~ "*)"

}
