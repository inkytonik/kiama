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
package L3

/**
 * Parsers for L3 language.
 */
trait SyntaxAnalyser extends L2.SyntaxAnalyser {

    import base.source.{Declaration, Statement}
    import source.{Call, FPSection, ProcDecl, ValMode, VarMode}
    import scala.collection.immutable.Seq

    override def declarationsDef : PackratParser[Seq[Declaration]] =
        super.declarationsDef ~ rep (procedureDeclaration <~ ";") ^^ {
            case ds ~ pds => ds ++ pds
        }

    lazy val procedureDeclaration =
        ("PROCEDURE" ~> idndef) ~ (optformalParameters <~ ";") ~ block ~ idnuse ^^ ProcDecl

    lazy val optformalParameters : PackratParser[Seq[FPSection]] =
        "(" ~> repsep (fpsection, ";") <~ ")" |
        result (Nil)

    lazy val fpsection =
        optvar ~ (idndeflist <~ ":") ~ typedef ^^ FPSection

    lazy val optvar =
        "VAR" ^^^ VarMode () |
        result (ValMode ())

    override def statementDef : PackratParser[Statement] =
        procedureCall |
        super.statementDef

    lazy val procedureCall =
        idnuse ~ optActualParameters ^^ Call

    lazy val optActualParameters =
        "(" ~> repsep (expression, ",") <~ ")" |
        guard (";" | "ELSE" | "END") ^^^ Nil

    override def keywordStrings : Seq[String] =
        "PROCEDURE" +: super.keywordStrings

}
