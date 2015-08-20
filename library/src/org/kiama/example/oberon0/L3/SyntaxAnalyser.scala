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
package L3

import org.kiama.util.Positions

/**
 * Parsers for L3 language.
 */
class SyntaxAnalyser (positions : Positions) extends L2.SyntaxAnalyser (positions) {

    import base.source.{Declaration, Statement}
    import source.{Call, FPSection, ProcDecl, ValMode, VarMode}

    override def declarationsDef : Parser[Vector[Declaration]] =
        super.declarationsDef ~ rep (procedureDeclaration <~ ";") ^^ {
            case ds ~ pds => ds ++ pds
        }

    lazy val procedureDeclaration =
        ("PROCEDURE" ~> idndef) ~ (optformalParameters <~ ";") ~ block ~ idnuse ^^ ProcDecl

    lazy val optformalParameters : Parser[Vector[FPSection]] =
        "(" ~> repsep (fpsection, ";") <~ ")" |
        success (Vector ())

    lazy val fpsection =
        optvar ~ (idndeflist <~ ":") ~ typedef ^^ FPSection

    lazy val optvar =
        "VAR" ^^ (_ => VarMode ()) |
        success (ValMode ())

    override def statementDef : Parser[Statement] =
        procedureCall |
        super.statementDef

    lazy val procedureCall =
        idnuse ~ optActualParameters ^^ Call

    lazy val optActualParameters =
        "(" ~> repsep (expression, ",") <~ ")" |
        guard (";" | "ELSE" | "END") ^^^ Vector ()

    override def keywordStrings : List[String] =
        "PROCEDURE" +: super.keywordStrings

}
