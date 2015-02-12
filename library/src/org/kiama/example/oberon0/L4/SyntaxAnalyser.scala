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

/**
 * Parsers for L4 language.
 */
trait SyntaxAnalyser extends L3.SyntaxAnalyser {

    import base.source.Expression
    import L0.source.TypeDef
    import scala.collection.immutable.Seq
    import source.{ArrayTypeDef, FieldExp, FieldIdn, FieldList, IndexExp,
        RecordTypeDef}

    override def typedefDef : PackratParser[TypeDef] =
        ("ARRAY" ~> expression) ~ ("OF" ~> typedef) ^^ ArrayTypeDef |
        "RECORD" ~> fieldlists <~ "END" ^^ RecordTypeDef |
        super.typedefDef

    lazy val fieldlists =
        rep1sep (fieldlist, ";") ^^ (_.flatten)

    lazy val fieldlist =
        (idnlist <~ ":") ~ typedef ^^ {
            case is ~ t => Some (FieldList (is, t))
        } |
        result (None)

    lazy val idnlist =
        rep1sep (ident, ",")

    override def lhsDef : PackratParser[Expression] =
        lhs ~ ("." ~> fldidn) ^^ FieldExp |
        lhs ~ ("[" ~> expression <~ "]") ^^ IndexExp |
        super.lhsDef

    lazy val fldidn =
        ident ^^ FieldIdn

    override def keywordStrings : Seq[String] =
        "ARRAY" +: "OF" +: "RECORD" +: super.keywordStrings

}
