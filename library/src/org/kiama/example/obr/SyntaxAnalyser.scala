/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2015 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2015 Dominic Verity, Macquarie University.
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
package example.obr

import org.kiama.util.PositionedParserUtilities

/**
 * Module containing parsers for the Obr language.
 */
class SyntaxAnalyser extends PositionedParserUtilities {

    import ObrTree._
    import org.kiama.util.Positions.dupPos
    import scala.collection.immutable.{HashSet, Seq}
    import scala.language.postfixOps

    override val whiteSpace = """(\s|\(\*(?:.|[\n\r])*?\*\))+""".r

    val reservedWords = HashSet(
          "PROGRAM", "INTEGER", "BEGIN", "END", "ELSE", "CONST", "VAR"
        , "BOOLEAN", "ARRAY", "OF", "RECORD", "EXIT", "RETURN", "IF", "THEN"
        , "LOOP", "WHILE", "DO", "FOR", "TO", "OR", "MOD", "AND", "TRUE"
        , "FALSE", "EXCEPTION", "RAISE", "TRY", "CATCH"
        )

    lazy val parser : Parser[ObrInt] =
        phrase (program)

    lazy val program : Parser[ObrInt] =
        ("PROGRAM" ~> ident) ~
        ("(" ~> rep1sep (parameterdecl, ";") <~ ")" <~ ":" <~ "INTEGER" <~ ";") ~
        declarations ~
        ("BEGIN" ~> statementseq <~ "END") ~
        (ident <~ ".") ^^
            { case i1 ~ ps ~ ds ~ ss ~ i2 =>
                  ObrInt (i1, ps ++ ds, ss, i2) }

    lazy val parameterdecl : Parser[Declaration] =
        idndef <~ ":" <~ "INTEGER" ^^ IntParam

    lazy val declarations : Parser[Seq[Declaration]] =
        constantdecls ~ variabledecls ^^ { case cs ~ vs => cs ++ vs }

    lazy val constantdecls : Parser[Seq[Declaration]] =
        opt ("CONST" ~> rep1 (constantdecl <~ ";")) ^^
            {
                case None     => Nil
                case Some (l) => l
            }

    lazy val variabledecls : Parser[Seq[Declaration]] =
        opt ("VAR" ~> rep1 (variabledecl <~ ";")) ^^
            {
                case None     => Nil
                case Some (l) => l
            }

    lazy val constantdecl : Parser[Declaration] =
        idndef ~ ("=" ~> signed) ^^ IntConst |
        idndef <~ ":" <~ "EXCEPTION" ^^ ExnConst

    lazy val variabledecl : Parser[Declaration] =
        idndef <~ ":" <~ "BOOLEAN" ^^ BoolVar |
        idndef <~ ":" <~ "INTEGER" ^^ IntVar |
        idndef ~ (":" ~> "ARRAY" ~> integer <~ "OF" <~ "INTEGER") ^^ ArrayVar |
        idndef ~ (":" ~> "RECORD" ~> (fielddecl+) <~ "END") ^^ RecordVar |
        // Extra clause to handle parsing the declaration of an enumeration variable
        idndef ~ (":" ~> "(" ~> rep1sep (idndef, ",") <~ ")") ^^
            { case i ~ cs => EnumVar (i, cs map EnumConst )}

    lazy val fielddecl : Parser[Identifier] =
        ident <~ ":" <~ "INTEGER" <~ ";"

    lazy val statementseq : Parser[Seq[Statement]] =
        statement*

    lazy val statement : Parser[Statement] =
        lvalue ~ mark (":=") ~ (expression <~ ";") ^^
            { case l ~ p ~ e => dupPos (p, AssignStmt (l, e)) } |
        conditional |
        iteration |
        trycatch |
        "EXIT" ~ ";" ^^ (_ => ExitStmt ()) |
        "RETURN" ~> expression <~ ";" ^^ ReturnStmt |
        "RAISE" ~> idnuse <~ ";" ^^ RaiseStmt

    lazy val conditional : Parser[IfStmt] =
        "IF" ~> expression ~ ("THEN" ~> statementseq) ~ optelseend ^^ IfStmt

    lazy val optelseend : Parser[Seq[Statement]] =
        "ELSE" ~> statementseq <~ "END" |
        "END" ^^^ Nil

    lazy val iteration : Parser[Statement] =
        "LOOP" ~> statementseq <~ "END" ^^ LoopStmt |
        "WHILE" ~> expression ~ ("DO" ~> statementseq <~ "END") ^^ WhileStmt |
        "FOR" ~> idnuse ~ (":=" ~> expression) ~ ("TO" ~> expression) ~
             ("DO" ~> statementseq <~ "END") ^^ ForStmt

    lazy val trycatch : Parser[TryStmt] =
        mark ("TRY") ~ statementseq ~ ((catchclause*) <~ "END") ^^
            { case p ~ ss ~ cs =>
                val body = dupPos (p, TryBody (ss))
                TryStmt (body, cs) }

    lazy val catchclause : Parser[Catch] =
        ("CATCH" ~> idnuse <~ "DO") ~ statementseq ^^ Catch

    lazy val expression : PackratParser[Expression] =
        expression ~ mark ("=") ~ simplexp ^^
            { case e ~ p ~ s => dupPos (p, EqualExp (e, s)) } |
        expression ~ mark ("#") ~ simplexp ^^
            { case e ~ p ~ s => dupPos (p, NotEqualExp (e, s)) } |
        expression ~ mark ("<") ~ simplexp ^^
            { case e ~ p ~ s => dupPos (p, LessExp (e, s)) } |
        expression ~ mark (">") ~ simplexp ^^
            { case e ~ p ~ s => dupPos (p, GreaterExp (e, s)) } |
        simplexp

    lazy val simplexp : PackratParser[Expression] =
        simplexp ~ mark ("+") ~ term ^^
            { case s ~ p ~ t => dupPos (p, PlusExp (s, t)) } |
        simplexp ~ mark ("-") ~ term ^^
            { case s ~ p ~ t => dupPos (p, MinusExp (s, t)) } |
        simplexp ~ mark ("OR") ~ term ^^
            { case s ~ p ~ t => dupPos (p, OrExp (s, t)) } |
        term

    lazy val term : PackratParser[Expression] =
        term ~ mark ("*") ~ factor ^^
            { case t ~ p ~ f => dupPos (p, StarExp (t, f)) } |
        term ~ mark ("/") ~ factor ^^
            { case t ~ p ~ f => dupPos (p, SlashExp (t, f)) } |
        term ~ mark ("MOD") ~ factor ^^
            { case t ~ p ~ f => dupPos (p, ModExp (t, f)) } |
        term ~ mark ("AND") ~ factor ^^
            { case t ~ p ~ f => dupPos (p, AndExp (t, f)) } |
        factor

    lazy val factor : PackratParser[Expression] =
        "TRUE" ^^ (_ => BoolExp (true)) |
        "FALSE" ^^ (_ => BoolExp (false)) |
        lvalue |
        integer ^^ IntExp |
        "(" ~> expression <~ ")" |
        "~" ~> factor ^^ NotExp |
        "-" ~> factor ^^ NegExp

    lazy val lvalue : PackratParser[AssignTree] =
        idnuse ~ ("[" ~> expression <~ "]") ^^ IndexExp |
        idnuse ~ mark (".") ~ ident ^^
            { case r ~ p ~ f => dupPos (p, FieldExp (r, f)) } |
        idnuse ^^ IdnExp

    lazy val integer : PackratParser[Int] =
        "[0-9]+".r ^^ (s => s.toInt)

    lazy val signed : PackratParser[Int] =
        "-?[0-9]+".r ^^ (s => s.toInt)

    lazy val idndef : PackratParser[IdnDef] =
        ident ^^ IdnDef

    lazy val idnuse : PackratParser[IdnUse] =
        ident ^^ IdnUse

    lazy val ident : PackratParser[Identifier] =
        "[a-zA-Z][a-zA-Z0-9]*".r into (s => {
            if (reservedWords contains s)
                failure (s"""keyword "$s" found where variable name expected""")
            else
                success (s)
        })

}
