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

package org.kiama
package example.oberon0
package base

import org.kiama.parsing.Parsers
import org.kiama.util.Positions

/**
 * Parsers for base language.
 */
class SyntaxAnalyser (positions : Positions) extends Parsers (positions) {

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
            case ds1 ~ Block (ds2, ss) =>
                Block (ds1 ++ ds2, ss)
        }

    lazy val declarations =
        declarationsDef

    def declarationsDef : Parser[Vector[Declaration]] =
        "" ^^^ Vector ()

    lazy val statements =
        "BEGIN" ~> statementSequence <~ "END" |
        "END" ^^ (_ => Block (Vector (), Vector ()))

    lazy val statementSequence =
        rep1sep (statement, ";") ^^ {
            case ss =>
                Block (Vector (), ss)
        }

    lazy val statement =
        statementDef

    def statementDef : Parser[Statement] =
        success (EmptyStmt ())

    lazy val idndef =
        ident ^^ IdnDef

    lazy val idnuse =
        ident ^^ IdnUse

    def keywordStrings : List[String] =
        List ("BEGIN", "END", "MODULE")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9]".r, keywordStrings)

    lazy val ident =
        not (keyword) ~> "[a-zA-Z_][a-zA-Z0-9]*".r |
        failure ("ident expected")

    override val whitespace =
        rep ("""\s+""".r | comment)

    lazy val comment : Parser[Any] =
        "(*" ~ rep (not ("*)") ~ (comment | any)) ~ "*)"

}
