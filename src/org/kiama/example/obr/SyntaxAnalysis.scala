/**
 * Obr language parser.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2011 Dominic Verity, Macquarie University.
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

package org.kiama.example.obr

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Module containing parsers for the Obr language.
 */
class SyntaxAnalysis extends RegexParsers with PackratParsers {

    import ObrTree._
    import scala.collection.immutable.HashSet
    import scala.util.parsing.input.Positional

    override val whiteSpace = """(\s|\(\*(?:.|[\n\r])*?\*\))+""".r

    val reservedWords = HashSet(
          "PROGRAM", "INTEGER", "BEGIN", "END", "ELSE", "CONST", "VAR"
        , "BOOLEAN", "ARRAY", "OF", "RECORD", "EXIT", "RETURN", "IF", "THEN"
        , "LOOP", "WHILE", "DO", "FOR", "TO", "OR", "MOD", "AND", "TRUE"
        , "FALSE", "EXCEPTION", "RAISE", "TRY", "CATCH"
        )

    case class Pos(s : String) extends Positional {
        override def toString : String = s
    }

    def withPos (op : Parser[String]) : Parser[Pos] = positioned (op ^^ Pos)

    lazy val parser : Parser[ObrInt] =
        phrase (program)

    lazy val program : Parser[ObrInt] = positioned (
        ("PROGRAM" ~> ident) ~
        ("(" ~> rep1sep (parameterdecl, ";") <~ ")" <~ ":" <~ "INTEGER" <~ ";") ~
        declarations ~
        ("BEGIN" ~> statementseq <~ "END") ~
        (ident <~ ".") ^^
            { case i1 ~ ps ~ ds ~ ss ~ i2 =>
                  ObrInt (i1, ps ++ ds, ss, i2) })

    lazy val parameterdecl : Parser[Declaration] = positioned(
        ident <~ ":" <~ "INTEGER" ^^ IntParam )

    lazy val declarations : Parser[List[Declaration]] =
        constantdecls ~ variabledecls ^^ { case cs ~ vs => cs ++ vs }

    lazy val constantdecls : Parser[List[Declaration]] =
        opt ("CONST" ~> rep1 (constantdecl <~ ";")) ^^
        	{
        	    case None     => Nil
                case Some (l) => l
            }

    lazy val variabledecls : Parser[List[Declaration]] =
        opt ("VAR" ~> rep1 (variabledecl <~ ";")) ^^
        	{
        	    case None     => Nil
                case Some (l) => l
            }

    lazy val constantdecl : Parser[Declaration] = positioned (
        ident ~ ("=" ~> signed) ^^ { case i ~ e => IntConst (i, e) } |
        ident <~ ":" <~ "EXCEPTION" ^^ ExnConst )

    lazy val variabledecl : Parser[Declaration] = positioned (
        ident <~ ":" <~ "BOOLEAN" ^^ BoolVar |
        ident <~ ":" <~ "INTEGER" ^^ IntVar |
        ident ~ (":" ~> "ARRAY" ~> integer <~ "OF" <~ "INTEGER") ^^
        	{ case i ~ v => ArrayVar (i, v) } |
        ident ~ (":" ~> "RECORD" ~> (fielddecl+) <~ "END") ^^
            { case i ~ fs => RecordVar (i, fs) } |
        // Extra clause to handle parsing the declaration of an enumeration variable
        ident ~ (":" ~> "(" ~> rep1sep (withPos (ident), ",") <~ ")") ^^
            { case i ~ cs => EnumVar (i, cs map { case p @ Pos (s) => EnumConst (s) setPos p.pos }) })

    lazy val fielddecl : Parser[Identifier] =
    	ident <~ ":" <~ "INTEGER" <~ ";"

    lazy val statementseq : Parser[List[Statement]] =
        statement*

    lazy val statement : Parser[Statement] =
        lvalue ~ withPos (":=") ~ (expression <~ ";") ^^
            { case l ~ p ~ e => AssignStmt (l, e) setPos p.pos } |
        conditional |
        iteration |
        trycatch |
        positioned ("EXIT" ~ ";" ^^^ ExitStmt ()) |
        positioned ("RETURN" ~> expression <~ ";" ^^ ReturnStmt) |
        positioned ("RAISE" ~> ident <~ ";" ^^ RaiseStmt)

    lazy val conditional : Parser[IfStmt] = positioned (
        "IF" ~> expression ~ ("THEN" ~> statementseq) ~ optelseend ^^
             { case e ~ ss ~ oee => IfStmt (e, ss, oee)} )

    lazy val optelseend : Parser[List[Statement]] =
    	"ELSE" ~> statementseq <~ "END" |
        "END" ^^^ Nil

    lazy val iteration : Parser[Statement] = positioned (
        "LOOP" ~> statementseq <~ "END" ^^ LoopStmt |
        "WHILE" ~> expression ~ ("DO" ~> statementseq <~ "END") ^^
             { case e ~ ss => WhileStmt (e, ss) } |
        "FOR" ~> ident ~ (":=" ~> expression) ~ ("TO" ~> expression) ~
             ("DO" ~> statementseq <~ "END") ^^
                 { case i ~ e1 ~ e2 ~ ss => ForStmt (i, e1, e2, ss) } )

    lazy val trycatch : Parser[TryStmt] = positioned (
        withPos ("TRY") ~ statementseq ~ ((catchclause*) <~ "END") ^^
            { case p ~ ss ~ cs =>
                val body = TryBody (ss) setPos p.pos
                TryStmt (body, cs) } )

    lazy val catchclause : Parser[Catch] = positioned (
        ("CATCH" ~> ident <~ "DO") ~ statementseq ^^
            { case i ~ ss => Catch (i, ss) })

    lazy val expression : PackratParser[Expression] =
        expression ~ withPos ("=") ~ simplexp ^^
            { case e ~ p ~ s => EqualExp (e, s) setPos p.pos } |
        expression ~ withPos ("#") ~ simplexp ^^
            { case e ~ p ~ s => NotEqualExp (e, s) setPos p.pos } |
        expression ~ withPos ("<") ~ simplexp ^^
            { case e ~ p ~ s => LessExp (e, s) setPos p.pos } |
        expression ~ withPos (">") ~ simplexp ^^
            { case e ~ p ~ s => GreaterExp (e, s) setPos p.pos } |
        simplexp

    lazy val simplexp : PackratParser[Expression] =
        simplexp ~ withPos ("+") ~ term ^^
            { case s ~ p ~ t => PlusExp (s, t) setPos p.pos } |
        simplexp ~ withPos ("-") ~ term ^^
            { case s ~ p ~ t => MinusExp (s, t) setPos p.pos } |
        simplexp ~ withPos ("OR") ~ term ^^
            { case s ~ p ~ t => OrExp (s, t) setPos p.pos } |
        term

    lazy val term : PackratParser[Expression] =
        term ~ withPos ("*") ~ factor ^^
            { case t ~ p ~ f => StarExp (t, f) setPos p.pos } |
        term ~ withPos ("/") ~ factor ^^
            { case t ~ p ~ f => SlashExp (t, f) setPos p.pos } |
        term ~ withPos ("MOD") ~ factor ^^
            { case t ~ p ~ f => ModExp (t, f) setPos p.pos } |
        term ~ withPos ("AND") ~ factor ^^
            { case t ~ p ~ f => AndExp (t, f) setPos p.pos } |
        factor

    lazy val factor : PackratParser[Expression] =
        positioned ("TRUE" ^^^ BoolExp (true)) |
        positioned ("FALSE" ^^^ BoolExp (false)) |
        lvalue |
        positioned (integer ^^ IntExp) |
        "(" ~> expression <~ ")" |
        positioned ("~" ~> factor ^^ NotExp) |
        positioned ("-" ~> factor ^^ NegExp)

    lazy val lvalue : PackratParser[AssignNode] =
        positioned (ident ~ ("[" ~> expression <~ "]") ^^
            { case a ~ e => IndexExp (a, e) }) |
        ident ~ withPos (".") ~ ident ^^
            { case r ~ p ~ f => FieldExp (r, f) setPos p.pos } |
        positioned (ident ^^ IdnExp)

    lazy val integer : PackratParser[Int] =
        "[0-9]+".r ^^ (s => s.toInt)

    lazy val signed : PackratParser[Int] =
        "-?[0-9]+".r ^^ (s => s.toInt)

    lazy val ident : PackratParser[Identifier] =
        "[a-zA-Z][a-zA-Z0-9]*".r into (s => {
            if (reservedWords contains s)
                failure ("keyword \"" + s + "\" found where variable name expected")
            else
                success (s)
        })

}
