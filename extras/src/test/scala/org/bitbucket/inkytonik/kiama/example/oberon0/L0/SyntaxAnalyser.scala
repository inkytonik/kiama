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
package L0

import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parsers for L0 language.
 */
class SyntaxAnalyser(positions : Positions) extends base.SyntaxAnalyser(positions) {

    import base.source.{Declaration, Expression, Statement}
    import scala.language.postfixOps
    import source.{
        AddExp,
        AndExp,
        Assignment,
        ConstDecl,
        DivExp,
        EqExp,
        GeExp,
        GtExp,
        IdnExp,
        IntExp,
        LeExp,
        LtExp,
        ModExp,
        MulExp,
        NamedType,
        NeExp,
        NegExp,
        NotExp,
        OrExp,
        SubExp,
        TypeDecl,
        TypeDef,
        VarDecl
    }

    override def declarationsDef : Parser[Vector[Declaration]] =
        (constdeclsection?) ~ (typedeclsection?) ~ (vardeclsection?) ^^ {
            case oc ~ ot ~ ov =>
                Vector(oc, ot, ov).flatten.flatten
        }

    lazy val constdeclsection =
        "CONST" ~> rep(constdecl)

    lazy val constdecl =
        (idndef <~ "=") ~ (expression <~ ";") ^^ ConstDecl

    lazy val typedeclsection =
        "TYPE" ~> rep(typedecl)

    lazy val typedecl =
        (idndef <~ "=") ~ (typedef <~ ";") ^^ TypeDecl

    lazy val vardeclsection =
        "VAR" ~> rep(vardecl)

    lazy val vardecl =
        (idndeflist <~ ":") ~ (typedef <~ ";") ^^ VarDecl

    lazy val idndeflist =
        rep1sep(idndef, ",")

    lazy val typedef =
        typedefDef

    def typedefDef : Parser[TypeDef] =
        namedtypedef

    lazy val namedtypedef =
        idnuse ^^ NamedType

    override def statementDef : Parser[Statement] =
        assignment |
            super.statementDef

    lazy val assignment =
        lhs ~ (":=" ~> expression) ^^ Assignment

    lazy val lhs =
        lhsDef

    def lhsDef : PackratParser[Expression] =
        idnuse ^^ IdnExp

    lazy val expression : PackratParser[Expression] =
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
            "(" ~> expression <~ ")" |
            failure("expression expected")

    lazy val intexp =
        constrainedInt ^^ IntExp

    override def keywordStrings : List[String] =
        "CONST" +: "DIV" +: "MOD" +: "OR" +: "TYPE" +: "VAR" +: super.keywordStrings

}
