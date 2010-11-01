/**
 * Obr language parser.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Module containing parsers for the expression language.
 */
trait SyntaxAnalysis extends RegexParsers with PackratParsers {

    import ObrTree._

    lazy val parser : PackratParser[ObrInt] =
        phrase (program)

    lazy val program : PackratParser[ObrInt] =
        ("PROGRAM" ~> ident) ~
        ("(" ~> rep1sep (parameterdecl, ";") <~ ")" <~ ":" <~ "INTEGER" <~ ";") ~
        declarations ~
        ("BEGIN" ~> statementseq <~ "END") ~
        (ident <~ ".") ^^
            { case i1 ~ ps ~ ds ~ ss ~ i2 =>
                  ObrInt (i1, ps ++ ds, ss, i2) }

    lazy val parameterdecl : PackratParser[Declaration] =
        ident <~ ":" <~ "INTEGER" ^^ IntParam

    lazy val declarations : PackratParser[List[Declaration]] =
        constantdecls ~ variabledecls ^^ { case cs ~ vs => cs ++ vs }

    lazy val constantdecls : PackratParser[List[Declaration]] =
        opt ("CONST" ~> rep1 (constantdecl <~ ";")) ^^
            {
                case None     => Nil
                case Some (l) => l
            }

    lazy val variabledecls : PackratParser[List[Declaration]] =
        opt ("VAR" ~> rep1 (variabledecl <~ ";")) ^^
            {
                case None     => Nil
                case Some (l) => l
            }

    lazy val constantdecl : PackratParser[Declaration] =
        ident ~ ("=" ~> integer) ^^ { case i ~ e => IntConst (i, e) }

    lazy val variabledecl : PackratParser[Declaration] =
        ident <~ ":" <~ "BOOLEAN" ^^ BoolVar |
        ident <~ ":" <~ "INTEGER" ^^ IntVar |
        ident ~ (":" ~> "ARRAY" ~> integer <~ "OF" <~ "INTEGER") ^^
            { case i ~ v => ArrayVar (i, v) } |
        ident ~ (":" ~> "RECORD" ~> (fielddecl+) <~ "END") ^^
            { case i ~ fs => RecordVar (i, fs) }

    lazy val fielddecl : PackratParser[Identifier] =
        ident <~ ":" <~ "INTEGER" <~ ";"

    lazy val statementseq : PackratParser[List[Statement]] =
        statement*

    lazy val statement : PackratParser[Statement] =
        lvalue ~ (":=" ~> expression <~ ";") ^^ { case l ~ e => AssignStmt (l, e) } |
        conditional |
        iteration |
        "EXIT" ~ ";" ^^^ ExitStmt () |
        "RETURN" ~> expression <~ ";" ^^ ReturnStmt

    lazy val conditional : PackratParser[IfStmt] =
        "IF" ~> expression ~ ("THEN" ~> statementseq) ~ optelseend ^^
             { case e ~ ss ~ oee => IfStmt (e, ss, oee)}

    lazy val optelseend : PackratParser[List[Statement]] =
        "ELSE" ~> statementseq <~ "END" |
        "END" ^^^ Nil

    lazy val iteration : PackratParser[Statement] =
        "LOOP" ~> statementseq <~ "END" ^^ LoopStmt |
        "WHILE" ~> expression ~ ("DO" ~> statementseq <~ "END") ^^
             { case e ~ ss => WhileStmt (e, ss) } |
        "FOR" ~> ident ~ (":=" ~> expression) ~ ("TO" ~> expression) ~
             ("DO" ~> statementseq <~ "END") ^^
                 { case i ~ e1 ~ e2 ~ ss => ForStmt (i, e1, e2, ss) }

    lazy val expression : PackratParser[Expression] =
        expression ~ ("=" ~> simplexp) ^^ { case e ~ s => EqualExp (e, s) } |
        expression ~ ("#" ~> simplexp) ^^ { case e ~ s => NotEqualExp (e, s) } |
        expression ~ ("<" ~> simplexp) ^^ { case e ~ s => LessExp (e, s) } |
        expression ~ (">" ~> simplexp) ^^ { case e ~ s => GreaterExp (e, s) } |
        simplexp

    lazy val simplexp : PackratParser[Expression] =
        simplexp ~ ("+" ~> term) ^^ { case s ~ t => PlusExp (s, t) } |
        simplexp ~ ("-" ~> term) ^^ { case s ~ t => MinusExp (s, t) } |
        simplexp ~ ("OR" ~> term) ^^ { case s ~ t => OrExp (s, t) } |
        term

    lazy val term : PackratParser[Expression] =
        term ~ ("*" ~> factor) ^^ { case t ~ f => StarExp (t, f) } |
        term ~ ("/" ~> factor) ^^ { case t ~ f => SlashExp (t, f) } |
        term ~ ("MOD" ~> factor) ^^ { case t ~ f => ModExp (t, f) } |
        term ~ ("AND" ~> factor) ^^ { case t ~ f => AndExp (t, f) } |
        factor

    lazy val factor : PackratParser[Expression] =
        "TRUE" ^^^ BoolExp (true) |
        "FALSE" ^^^ BoolExp (false) |
        lvalue |
        integer ^^ IntExp |
        "(" ~> expression <~ ")" |
        "~" ~> factor ^^ NotExp |
        "-" ~> factor ^^ NegExp

    lazy val lvalue : PackratParser[AssignNode] =
        ident ~ ("[" ~> expression <~ "]") ^^ { case a ~ e => IndexExp (a, e) } |
        ident ~ ("." ~> ident) ^^ { case r ~ f => FieldExp (r, f) } |
        ident ^^ IdnExp

    lazy val integer : PackratParser[Int] =
        "[0-9]+".r ^^ (s => s.toInt)

    lazy val ident : PackratParser[Identifier] =
        regex ("[a-zA-Z]+".r)

    override protected val whiteSpace =
        """(\s|(\(\*(?:.|[\n\r])*?\*\)))+""".r

}
