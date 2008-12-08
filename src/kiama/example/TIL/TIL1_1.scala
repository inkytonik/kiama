/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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
                                
package kiama.example.til

import kiama.parsing.CharPackratParsers

/**
 * Parser for the Tiny Imperative Language.
 */
object TIL1_1 extends CharPackratParsers {
    
    import AST._

    val parse =
        phrase (program)

    lazy val program = (statement*) ^^ Program 
    
    lazy val statement : Parser[Stat] =
        declaration | assignment_statement | if_statement | while_statement |
        for_statement | read_statement | write_statement

    lazy val declaration = "var" ~> identifier <~ ";" ^^ Decl

    lazy val assignment_statement =
        identifier ~ (":=" ~> expression <~ ";") ^^
            { case i ~ e => Assign (i, e) }

    lazy val if_statement =
        ("if" ~> expression) ~ ("then" ~> (statement*)) ~ ("else" ~> (statement*) <~ "end") ^^
            { case c ~ t ~ e => IfElse (c, t, e) } |
        "if" ~> expression ~ ("then" ~> (statement*) <~ "end") ^^
            { case c ~ t => IfThen (c, t) }
        
    lazy val while_statement =
        ("while" ~> expression <~ "do") ~ (statement*) <~ "end" ^^
            { case e ~ b => While (e, b) }

    lazy val for_statement =
        ("for" ~> identifier) ~ (":=" ~> expression) ~ ("to" ~> expression) ~ ("do" ~> (statement*) <~ "end") ^^
            { case i ~ f ~ t ~ b => For (i, f, t, b) }

    lazy val read_statement = "read" ~> identifier <~ ";" ^^ Read

    lazy val write_statement = "write" ~> expression <~ ";" ^^ Write

    lazy val expression : MemoParser[Exp] =
        expression ~ ("=" ~> term) ^^ { case e ~ t => Eq (e, t) } |
        expression ~ ("!=" ~> term) ^^ { case e ~ t => Ne (e, t) } |
        term

    lazy val term : MemoParser[Exp] =
        term ~ ("+" ~> factor) ^^ { case t ~ f => Add (t, f) } |
        term ~ ("-" ~> factor) ^^ { case t ~ f => Sub (t, f) } |
        factor

    lazy val factor : MemoParser[Exp] =
        factor ~ ("*" ~> primary) ^^ { case f ~ p => Mul (f, p) } |
        factor ~ ("/" ~> primary) ^^ { case f ~ p => Div (f, p) } |
        primary

    lazy val primary : MemoParser[Exp] =
        identifier ^^ Var |
        integer |
        string |
        "(" ~> expression <~ ")"
        
    lazy val keyword = "var" | "if" | "then" | "else" | "while" | "do" |
        "for" | "read" | "write"

    lazy val identifier =
        !keyword ~> token (letter ~ (letterOrDigit*)) ^^
            { case c ~ cs => c + cs.mkString }
        
    lazy val integer =
        token (digit+) ^^ (l => Num (l.mkString.toInt))

    lazy val string =
        token ('"' ~> """[^\"]+""".r <~ '"') ^^ Str

}
