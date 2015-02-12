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
package L0

/**
 * Parsers for L0 language.
 */
trait SyntaxAnalyser extends base.SyntaxAnalyser {

    import base.source.{Declaration, Expression, Statement}
    import scala.collection.immutable.Seq
    import scala.language.postfixOps
    import source.{AddExp, AndExp, Assignment, ConstDecl, DivExp, EqExp,
        GeExp, GtExp, IdnExp, IntExp, LeExp, LtExp, ModExp, MulExp,
        NamedType, NeExp, NegExp, NotExp, OrExp, SubExp, TypeDecl,
        TypeDef, VarDecl}

    override def declarationsDef : PackratParser[Seq[Declaration]] =
        (constdeclsection?) ~ (typedeclsection?) ~ (vardeclsection?) ^^ {
            case oc ~ ot ~ ov =>
                Seq (oc, ot, ov).flatten.flatten
        }

    lazy val constdeclsection =
        "CONST" ~> rep (constdecl)

    lazy val constdecl =
        (idndef <~ "=") ~ (expression <~ ";") ^^ ConstDecl

    lazy val typedeclsection =
        "TYPE" ~> rep (typedecl)

    lazy val typedecl =
        (idndef <~ "=") ~ (typedef <~ ";") ^^ TypeDecl

    lazy val vardeclsection =
        "VAR" ~> rep (vardecl)

    lazy val vardecl =
        (idndeflist <~ ":") ~ (typedef <~ ";") ^^ VarDecl

    lazy val idndeflist =
        rep1sep (idndef, ",")

    lazy val typedef =
        typedefDef

    def typedefDef : PackratParser[TypeDef] =
        namedtypedef

    lazy val namedtypedef =
        idnuse ^^ NamedType

    override def statementDef : PackratParser[Statement] =
        assignment |
        super.statementDef

    lazy val assignment =
        lhs ~ (":=" ~> expression) ^^ Assignment

    lazy val lhs =
        lhsDef

    def lhsDef : PackratParser[Expression] =
        idnuse ^^ IdnExp

    lazy val expression =
        simpexp ~ ("=" ~> simpexp) ^^ EqExp |
        simpexp ~ ("#" ~> simpexp) ^^ NeExp |
        simpexp ~ ("<" ~> simpexp) ^^ LtExp |
        simpexp ~ ("<=" ~> simpexp) ^^ LeExp |
        simpexp ~ (">" ~> simpexp) ^^ GtExp |
        simpexp ~ (">=" ~> simpexp) ^^ GeExp |
        simpexp

    lazy val simpexp : PackratParser[Expression] =
        simpexp ~ ("+" ~> term) ^^ AddExp |
        simpexp ~ ("-" ~> term) ^^ SubExp |
        simpexp ~ ("OR" ~> term) ^^ OrExp |
        term

    lazy val term : PackratParser[Expression] =
        term ~ ("*" ~> factor) ^^ MulExp |
        term ~ ("DIV" ~> factor) ^^ DivExp |
        term ~ ("MOD" ~> factor) ^^ ModExp |
        term ~ ("&" ~> factor) ^^ AndExp |
        factor

    lazy val factor : PackratParser[Expression] =
        intexp |
        lhs |
        "+" ~> factor |
        "-" ~> factor ^^ NegExp |
        "~" ~> factor ^^ NotExp |
        "(" ~> expression <~ ")"

    lazy val intexp =
        constrainedInt ^^ IntExp

    override def keywordStrings : Seq[String] =
        "CONST" +: "DIV" +: "MOD" +: "OR" +: "TYPE" +: "VAR" +: super.keywordStrings

}
