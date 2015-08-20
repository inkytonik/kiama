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
package L4

import org.kiama.util.Positions

/**
 * Parsers for L4 language.
 */
class SyntaxAnalyser (positions: Positions) extends L3.SyntaxAnalyser (positions) {

    import base.source.Expression
    import L0.source.TypeDef
    import source.{ArrayTypeDef, FieldExp, FieldIdn, Fields, IndexExp, RecordTypeDef}

    override def typedefDef : Parser[TypeDef] =
        ("ARRAY" ~> expression) ~ ("OF" ~> typedef) ^^ ArrayTypeDef |
        "RECORD" ~> fieldlists <~ "END" ^^ RecordTypeDef |
        super.typedefDef

    lazy val fieldlists =
        rep1sep (fieldlist, ";") ^^ (_.flatten)

    lazy val fieldlist =
        (idnlist <~ ":") ~ typedef ^^ {
            case is ~ t => Some (Fields (is, t))
        } |
        success (None)

    lazy val idnlist =
        rep1sep (ident, ",")

    override def lhsDef : PackratParser[Expression] =
        lhs ~ ("." ~> fldidn) ^^ FieldExp |
        lhs ~ ("[" ~> expression <~ "]") ^^ IndexExp |
        super.lhsDef

    lazy val fldidn =
        ident ^^ FieldIdn

    override def keywordStrings : List[String] =
        "ARRAY" +: "OF" +: "RECORD" +: super.keywordStrings

}
