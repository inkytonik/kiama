/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package example.til

import org.kiama.util.{Positioned, PositionedParserUtilities}

/**
 * AST the basic Tiny Imperative Language.
 */
object AST {

    import scala.collection.immutable.Seq

    case class Program (ss : Seq[Stat])

    sealed abstract class Stat extends Positioned

    case class Decl (i : Id) extends Stat

    case class Assign (i : Id, e : Exp) extends Stat

    case class IfThen (e : Exp, t : Seq[Stat]) extends Stat
    case class IfElse (e : Exp, t : Seq[Stat], f : Seq[Stat]) extends Stat

    case class While (e : Exp, b : Seq[Stat]) extends Stat
    case class For (i : Id, f : Exp, t : Exp, b : Seq[Stat]) extends Stat

    case class Read (i : Id) extends Stat
    case class Write (e : Exp) extends Stat

    sealed abstract class Exp

    case class Id (s : String) {
        override def toString : String = s"""Id ("$s")"""
    }

    case class Var (i : Id) extends Exp
    case class Num (n : Int) extends Exp
    case class Str (s : String) extends Exp {
        override def toString : String = s"""Str ("$s")"""
    }

    case class Mul (l : Exp, r : Exp) extends Exp
    case class Div (l : Exp, r : Exp) extends Exp
    case class Add (l : Exp, r : Exp) extends Exp
    case class Sub (l : Exp, r : Exp) extends Exp

    case class Eq (l : Exp, r : Exp) extends Exp
    case class Ne (l : Exp, r : Exp) extends Exp

}

/**
 * Parser for the basic Tiny Imperative Language.
 */
trait TIL1_1 extends PositionedParserUtilities {

    import AST._
    import scala.language.postfixOps
    import scala.collection.immutable.Seq

    type Root = Program

    lazy val parser = program

    lazy val program = (statement*) ^^ Program

    lazy val statement : PackratParser[Stat] =
        declaration | assignment_statement | if_statement | while_statement |
        for_statement | read_statement | write_statement

    lazy val declaration = "var" ~> identifier <~ ";" ^^ Decl

    lazy val assignment_statement =
        identifier ~ (":=" ~> expression <~ ";") ^^ Assign

    lazy val if_statement =
        ("if" ~> expression) ~ ("then" ~> (statement*)) ~ ("else" ~> (statement*) <~ "end") ^^ IfElse |
        "if" ~> expression ~ ("then" ~> (statement*) <~ "end") ^^ IfThen

    lazy val while_statement =
        ("while" ~> expression <~ "do") ~ (statement*) <~ "end" ^^ While

    lazy val for_statement =
        ("for" ~> identifier) ~ (":=" ~> expression) ~ ("to" ~> expression) ~ ("do" ~> (statement*) <~ "end") ^^ For

    lazy val read_statement = "read" ~> identifier <~ ";" ^^ Read

    lazy val write_statement = "write" ~> expression <~ ";" ^^ Write

    lazy val expression : PackratParser[Exp] =
        expression ~ ("=" ~> term) ^^ Eq |
        expression ~ ("!=" ~> term) ^^ Ne |
        term

    lazy val term : PackratParser[Exp] =
        term ~ ("+" ~> factor) ^^ Add |
        term ~ ("-" ~> factor) ^^ Sub |
        factor

    lazy val factor : PackratParser[Exp] =
        factor ~ ("*" ~> primary) ^^ Mul |
        factor ~ ("/" ~> primary) ^^ Div |
        primary

    lazy val primary : PackratParser[Exp] =
        identifier ^^ Var |
        integer |
        string |
        "(" ~> expression <~ ")"

    lazy val keyword =
        keywords ("[^a-zA-Z0-9]".r,
                  Seq ("var", "if", "then", "else", "while", "do",
                       "for", "read", "write"))

    lazy val identifier =
        not (keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r ^^ Id

    lazy val integer =
        "[0-9]+".r ^^ (s => Num (s.toInt))

    lazy val string =
        """\"[^\"]+\"""".r ^^ Str

    override val whiteSpace =
        """(\s|(//.*\n))+""".r

}

object TIL1_1Main extends ParsingMain with TIL1_1

