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
package base

import org.kiama.util.WhitespacePositionedParserUtilities

/**
 * Parsers for base language.
 */
trait Parser extends WhitespacePositionedParserUtilities {

    import scala.collection.immutable.Seq
    import source.{Block, Declaration, EmptyStmt, IdnDef, IdnUse,
        ModuleDecl, Statement}

    lazy val parser =
        phrase (moduledecl)

    lazy val moduledecl =
        "MODULE" ~> (idndef <~ ";") ~ block ~ (idnuse <~ ".") ^^ ModuleDecl

    // Statement sequences produce blocks, which is not strictly necessary
    // for the source language but it makes transformation easier since
    // declarations can be added to the blocks later. At the moment there
    // will never be any declarations from parsing, but we leave this action
    // general in case that changes.

    lazy val block =
        declarations ~ statements ^^ {
            case ds1 ~ Block (ds2, ss) =>
                Block (ds1 ++ ds2, ss)
        }

    lazy val declarations =
        declarationsDef

    def declarationsDef : PackratParser[Seq[Declaration]]
        "" ^^^ Nil

    lazy val statements =
        "BEGIN" ~> statementSequence <~ "END" |
        "END" ^^^ Block (Nil, Nil)

    lazy val statementSequence =
        rep1sep (statement, ";") ^^ {
            case ss =>
                Block (Nil, ss)
        }

    lazy val statement =
        statementDef

    def statementDef : PackratParser[Statement] =
        result (EmptyStmt ())

    lazy val idndef =
        ident ^^ IdnDef

    lazy val idnuse =
        ident ^^ IdnUse

    def keywordStrings : Seq[String] =
        Seq ("BEGIN", "END", "MODULE")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9]".r, keywordStrings)

    lazy val ident =
        not (keyword) ~> "[a-zA-Z_][a-zA-Z0-9]*".r |
        failure ("ident expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep (whiteSpace | comment)

    lazy val comment : PackratParser[Any] =
        "(*" ~ rep (not ("*)") ~ (comment | any)) ~ "*)"

}
