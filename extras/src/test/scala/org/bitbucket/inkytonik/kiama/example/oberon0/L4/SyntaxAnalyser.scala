/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L4

import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parsers for L4 language.
 */
class SyntaxAnalyser(positions : Positions) extends L3.SyntaxAnalyser(positions) {

    import base.source.Expression
    import L0.source.TypeDef
    import source.{ArrayTypeDef, FieldExp, FieldIdn, Fields, IndexExp, RecordTypeDef}

    override def typedefDef : Parser[TypeDef] =
        ("ARRAY" ~> expression) ~ ("OF" ~> typedef) ^^ ArrayTypeDef |
            "RECORD" ~> fieldlists <~ "END" ^^ RecordTypeDef |
            super.typedefDef

    lazy val fieldlists =
        rep1sep(fieldlist, ";") ^^ (_.flatten)

    lazy val fieldlist =
        (idnlist <~ ":") ~ typedef ^^ {
            case is ~ t => Some(Fields(is, t))
        } |
            success(None)

    lazy val idnlist =
        rep1sep(ident, ",")

    override def lhsDef : PackratParser[Expression] =
        lhs ~ ("." ~> fldidn) ^^ FieldExp |
            lhs ~ ("[" ~> expression <~ "]") ^^ IndexExp |
            super.lhsDef

    lazy val fldidn =
        ident ^^ FieldIdn

    override def keywordStrings : List[String] =
        "ARRAY" +: "OF" +: "RECORD" +: super.keywordStrings

}
