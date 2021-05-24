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
package L3

import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parsers for L3 language.
 */
class SyntaxAnalyser(positions : Positions) extends L2.SyntaxAnalyser(positions) {

    import base.source.{Declaration, Statement}
    import source.{Call, FPSection, ProcDecl, ValMode, VarMode}

    override def declarationsDef : Parser[Vector[Declaration]] =
        super.declarationsDef ~ rep(procedureDeclaration <~ ";") ^^ {
            case ds ~ pds => ds ++ pds
        }

    lazy val procedureDeclaration =
        ("PROCEDURE" ~> idndef) ~ (optformalParameters <~ ";") ~ block ~ idnuse ^^ ProcDecl.apply

    lazy val optformalParameters : Parser[Vector[FPSection]] =
        "(" ~> repsep(fpsection, ";") <~ ")" |
            success(Vector())

    lazy val fpsection =
        optvar ~ (idndeflist <~ ":") ~ typedef ^^ FPSection.apply

    lazy val optvar =
        "VAR" ^^ (_ => VarMode()) |
            success(ValMode())

    override def statementDef : Parser[Statement] =
        procedureCall |
            super.statementDef

    lazy val procedureCall =
        idnuse ~ optActualParameters ^^ Call.apply

    lazy val optActualParameters =
        "(" ~> repsep(expression, ",") <~ ")" |
            guard(";" | "ELSE" | "END") ^^^ Vector()

    override def keywordStrings : List[String] =
        "PROCEDURE" +: super.keywordStrings

}
