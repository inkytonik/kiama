/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2021 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2021 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.obr

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for the Obr language.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import ObrTree._
    import scala.collection.immutable.HashSet

    override val whitespace : Parser[String] =
        """(\s|\(\*(?:.|[\n\r])*?\*\))*""".r

    val reservedWords = HashSet(
        "PROGRAM", "INTEGER", "BEGIN", "END", "ELSE", "CONST", "VAR", "BOOLEAN", "ARRAY", "OF", "RECORD", "EXIT", "RETURN", "IF", "THEN", "LOOP", "WHILE", "DO", "FOR", "TO", "OR", "MOD", "AND", "TRUE", "FALSE", "EXCEPTION", "RAISE", "TRY", "CATCH"
    )

    lazy val program =
        ("PROGRAM" ~> ident) ~
            ("(" ~> rep1sep(parameterdecl, ";") <~ ")" <~ ":" <~ "INTEGER" <~ ";") ~
            declarations ~
            ("BEGIN" ~> statementseq <~ "END") ~
            (ident <~ ".") ^^
            {
                case i1 ~ ps ~ ds ~ ss ~ i2 =>
                    ObrInt(i1, ps ++ ds, ss, i2)
            }

    lazy val parameterdecl : Parser[Declaration] =
        idndef <~ ":" <~ "INTEGER" ^^ IntParam.apply

    lazy val declarations : Parser[Vector[Declaration]] =
        constantdecls ~ variabledecls ^^ { case cs ~ vs => cs ++ vs }

    lazy val constantdecls : Parser[Vector[Declaration]] =
        opt("CONST" ~> rep1(constantdecl <~ ";")) ^^
            {
                case None    => Vector()
                case Some(l) => l
            }

    lazy val variabledecls : Parser[Vector[Declaration]] =
        opt("VAR" ~> rep1(variabledecl <~ ";")) ^^
            {
                case None    => Vector()
                case Some(l) => l
            }

    lazy val constantdecl : Parser[Declaration] =
        idndef ~ ("=" ~> signed) ^^ IntConst.apply |
            idndef <~ ":" <~ "EXCEPTION" ^^ ExnConst.apply

    lazy val variabledecl : Parser[Declaration] =
        idndef <~ ":" <~ "BOOLEAN" ^^ BoolVar.apply |
            idndef <~ ":" <~ "INTEGER" ^^ IntVar.apply |
            idndef ~ (":" ~> "ARRAY" ~> integer <~ "OF" <~ "INTEGER") ^^ ArrayVar.apply |
            idndef ~ (":" ~> "RECORD" ~> rep1(fielddecl) <~ "END") ^^ RecordVar.apply |
            // Extra clause to handle parsing the declaration of an enumeration variable
            idndef ~ (":" ~> "(" ~> rep1sep(idndef, ",") <~ ")") ^^
            { case i ~ cs => EnumVar(i, cs map EnumConst.apply) }

    lazy val fielddecl =
        ident <~ ":" <~ "INTEGER" <~ ";"

    lazy val statementseq =
        rep(statement)

    lazy val statement : Parser[Statement] =
        lvalue ~ mark(":=") ~ (expression <~ ";") ^^
            { case l ~ p ~ e => positions.dupPos(p, AssignStmt(l, e)) } |
            conditional |
            iteration |
            trycatch |
            "EXIT" ~ ";" ^^ (_ => ExitStmt()) |
            "RETURN" ~> expression <~ ";" ^^ ReturnStmt.apply |
            "RAISE" ~> idnuse <~ ";" ^^ RaiseStmt.apply

    lazy val conditional =
        "IF" ~> expression ~ ("THEN" ~> statementseq) ~ optelseend ^^ IfStmt.apply

    lazy val optelseend : Parser[Vector[Statement]] =
        "ELSE" ~> statementseq <~ "END" |
            "END" ^^^ Vector()

    lazy val iteration =
        "LOOP" ~> statementseq <~ "END" ^^ LoopStmt.apply |
            "WHILE" ~> expression ~ ("DO" ~> statementseq <~ "END") ^^ WhileStmt.apply |
            "FOR" ~> idnuse ~ (":=" ~> expression) ~ ("TO" ~> expression) ~
            ("DO" ~> statementseq <~ "END") ^^ ForStmt.apply

    lazy val trycatch =
        mark("TRY") ~ statementseq ~ (rep(catchclause) <~ "END") ^^
            {
                case p ~ ss ~ cs =>
                    val body = positions.dupPos(p, TryBody(ss))
                    TryStmt(body, cs)
            }

    lazy val catchclause =
        ("CATCH" ~> idnuse <~ "DO") ~ statementseq ^^ Catch.apply

    lazy val expression : PackratParser[Expression] =
        expression ~ mark("=") ~ simplexp ^^
            { case e ~ p ~ s => positions.dupPos(p, EqualExp(e, s)) } |
            expression ~ mark("#") ~ simplexp ^^
            { case e ~ p ~ s => positions.dupPos(p, NotEqualExp(e, s)) } |
            expression ~ mark("<") ~ simplexp ^^
            { case e ~ p ~ s => positions.dupPos(p, LessExp(e, s)) } |
            expression ~ mark(">") ~ simplexp ^^
            { case e ~ p ~ s => positions.dupPos(p, GreaterExp(e, s)) } |
            simplexp

    lazy val simplexp : PackratParser[Expression] =
        simplexp ~ mark("+") ~ term ^^
            { case s ~ p ~ t => positions.dupPos(p, PlusExp(s, t)) } |
            simplexp ~ mark("-") ~ term ^^
            { case s ~ p ~ t => positions.dupPos(p, MinusExp(s, t)) } |
            simplexp ~ mark("OR") ~ term ^^
            { case s ~ p ~ t => positions.dupPos(p, OrExp(s, t)) } |
            term

    lazy val term : PackratParser[Expression] =
        term ~ mark("*") ~ factor ^^
            { case t ~ p ~ f => positions.dupPos(p, StarExp(t, f)) } |
            term ~ mark("/") ~ factor ^^
            { case t ~ p ~ f => positions.dupPos(p, SlashExp(t, f)) } |
            term ~ mark("MOD") ~ factor ^^
            { case t ~ p ~ f => positions.dupPos(p, ModExp(t, f)) } |
            term ~ mark("AND") ~ factor ^^
            { case t ~ p ~ f => positions.dupPos(p, AndExp(t, f)) } |
            factor

    lazy val factor : Parser[Expression] =
        "TRUE" ^^ (_ => BoolExp(true)) |
            "FALSE" ^^ (_ => BoolExp(false)) |
            lvalue |
            integer ^^ IntExp.apply |
            "(" ~> expression <~ ")" |
            "~" ~> factor ^^ NotExp.apply |
            "-" ~> factor ^^ NegExp.apply

    lazy val lvalue : Parser[AssignTree] =
        idnuse ~ ("[" ~> expression <~ "]") ^^ IndexExp.apply |
            idnuse ~ mark(".") ~ ident ^^
            { case r ~ p ~ f => positions.dupPos(p, FieldExp(r, f)) } |
            idnuse ^^ IdnExp.apply

    lazy val integer =
        "[0-9]+".r ^^ (s => s.toInt)

    lazy val signed =
        "-?[0-9]+".r ^^ (s => s.toInt)

    lazy val idndef =
        ident ^^ IdnDef.apply

    lazy val idnuse =
        ident ^^ IdnUse.apply

    lazy val ident =
        "[a-zA-Z][a-zA-Z0-9]*".r into (s => {
            if (reservedWords contains s)
                failure(s"""keyword "$s" found where variable name expected""")
            else
                success(s)
        })

}
