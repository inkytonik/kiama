/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
 *
 * Contributed by Ben Mockler.
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
package example.oberon0.compiler

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * Parse Oberon-0 to an abstract syntax tree.
 */
trait Parser extends RegexParsers with PackratParsers {

    import AST._

    lazy val parser  : PackratParser[ModuleDecl] =
        phrase (moduledecl)

    // Declarations

    lazy val moduledecl : PackratParser[ModuleDecl] =
        "MODULE" ~> (ident <~ ";") ~ declarations ~
            (("BEGIN" ~> statementSequence)?) ~ ("END" ~> ident) <~ "." ^^
                { case id1 ~ decs ~ opstseq ~ id2 =>
                     ModuleDecl (id1.name, decs, optionalListToList (opstseq), id2.name, ModuleType()) }

    lazy val declarations : PackratParser[List[Declaration]] =
        (constdecls?) ~ (typedecls?) ~ (vardecls?) ~ procdecls  ^^
             { case c ~ t ~ v ~ p =>
                  optionalListToList (c) ::: optionalListToList (t) :::
                  optionalListToList (v) ::: p }

    lazy val constdecl : PackratParser[ConstDecl] =
        (ident <~ "=") ~ expression <~ ";" ^^ { case id ~ ex => ConstDecl (id.name, ex) }

    lazy val constdecls : PackratParser[List[ConstDecl]] =
        "CONST" ~> (constdecl*)

    lazy val typedecl : PackratParser[TypeDecl] =
        (ident <~ "=") ~ type1 <~ ";" ^^ {case id ~ tp => TypeDecl (id.name, tp) }

    lazy val typedecls : PackratParser[List[TypeDecl]] =
        "TYPE" ~> (typedecl*)

    lazy val vardeclspertype : PackratParser[List[VarDecl]] =
        (identList <~ ":") ~ type1 ^^
            { case lst ~ tp => lst.map (id => VarDecl (id.name, tp)) }

    lazy val vardecls : PackratParser[List[VarDecl]] =
        "VAR" ~> ((vardeclspertype <~ ";")*) ^^ (lst => lst.flatten)

    lazy val fpSection : PackratParser[List[Declaration]] =
        (("VAR"?) ~ identList <~ ":") ~ type1 ^^
           { case Some (_) ~ lst ~ tp => lst.map (id => RefVarDecl (id.name, tp))
             case None     ~ lst ~ tp => lst.map (id => VarDecl (id.name, tp)) }

    lazy val formalParameters : PackratParser[List[Declaration]] =
        "(" ~> repsep(fpSection, ";") <~ ")" ^^ (lst => lst.flatten)

    lazy val procdecls : PackratParser[List[ProcDecl]] =
        (procdecl <~ ";")*

    lazy val procdecl : PackratParser[ProcDecl] =
        ("PROCEDURE" ~> ident) ~ (formalParameters?) ~ (";" ~> declarations) ~
            (("BEGIN" ~> statementSequence)?) ~ ("END" ~> ident) ^^
                { case id1 ~ opfps ~ decs ~ opstseq ~ id2 =>
                     ProcDecl (id1.name, optionalListToList (opfps), decs,
                               optionalListToList (opstseq), id2.name, ProcType(optionalListToList (opfps))) }

    // Types

    lazy val type1 : PackratParser[Type] =
        "INTEGER" ^^ { case _ => IntegerType } |
        ident ^^ NamedType |
        arrayType |
        recordType

    lazy val identList : PackratParser[List[Ident]] =
        repsep (ident, ",")

    lazy val arrayType : PackratParser[ArrayType] =
        (("ARRAY" ~> expression) <~ "OF") ~ type1 ^^
            { case e ~ tp => ArrayType (e, tp) }

    lazy val fieldList : PackratParser[List[FieldDecl]] =
        (identList <~ ":") ~ type1 ^^
            { case lst ~ tp => lst.map (id => FieldDecl (id.name, tp)) }

    lazy val recordType : PackratParser[RecordType] =
        "RECORD" ~> repsep (fieldList, ";") <~ "END" ^^
            (lst => RecordType (lst.flatten))

    // Statements

    lazy val statementSequence : PackratParser[List[Statement]] =
        repsep (statement, ";")

    lazy val statement : PackratParser[Statement] =
        assignment |
        procedureCall |
        ifStatement |
        whileStatement

    lazy val assignment : PackratParser[Assignment] =
        (desig <~ ":=") ~ expression ^^ { case d ~ e => Assignment (d, e) }

    lazy val actualParameters : PackratParser[List[Exp]] =
        "(" ~> repsep (expression, ",") <~ ")"

    lazy val procedureCall : PackratParser[Statement] =
        desig ~ actualParameters ^^ { case d ~ aps => ProcedureCall (d, aps) } |
        desig ^^ (d => ProcedureCall (d, Nil))

    lazy val ifStatement : PackratParser[IfStatement] =
        ("IF" ~> expression) ~ ("THEN" ~> statementSequence) <~ "END" ^^
            { case con ~ thnss => IfStatement (con, thnss, Nil) } |
        ("IF" ~> expression) ~ ("THEN" ~> statementSequence) ~ ifTail ^^
            { case con ~ thnss ~ els => IfStatement (con, thnss, els) }

    lazy val ifTail : PackratParser[List[Statement]] =
        ("ELSIF" ~> expression) ~ ("THEN" ~> statementSequence) ~ ifTail ^^
            { case con ~ thnss ~ els => List (IfStatement (con, thnss, els)) } |
        ("ELSIF" ~> expression) ~ ("THEN" ~> statementSequence) <~ "END" ^^
            { case con ~ thnss => List (IfStatement (con, thnss, Nil)) } |
        ("ELSE" ~> statementSequence) <~ "END"

    lazy val whileStatement : PackratParser[WhileStatement] =
        (("WHILE" ~> expression) <~ "DO") ~ statementSequence <~ "END" ^^
            { case ex ~ ss => WhileStatement (ex, ss) }

    // Expressions

    lazy val expression : PackratParser[Exp] =
        (simpleExpression <~ "=") ~ simpleExpression ^^ { case se1 ~ se2 => Equal (se1, se2) } |
        (simpleExpression <~ "#") ~ simpleExpression ^^ { case se1 ~ se2 => NotEqual (se1, se2) } |
        (simpleExpression <~ "<") ~ simpleExpression ^^ { case se1 ~ se2 => LessThan (se1, se2) } |
        (simpleExpression <~ "<=") ~ simpleExpression ^^ { case se1 ~ se2 => LessThanOrEqual (se1, se2) } |
        (simpleExpression <~ ">") ~ simpleExpression ^^ { case se1 ~ se2 => GreaterThan (se1, se2) } |
        (simpleExpression <~ ">=") ~ simpleExpression ^^ { case se1 ~ se2 => GreaterThanOrEqual (se1, se2) } |
        simpleExpression

    lazy val simpleExpression : PackratParser[Exp] =
        (simpleExpression <~ "+") ~ term ^^ {case se ~ t => Plus (se, t) } |
        (simpleExpression <~ "-") ~ term ^^ {case se ~ t => Minus (se, t) } |
        (simpleExpression <~ "OR") ~ term ^^ {case se ~ t => Or (se, t) } |
        term

    lazy val term : PackratParser[Exp] =
        (term <~ "*") ~ factor ^^ { case t ~ f => Mult (t, f) } |
        (term <~ "DIV") ~ factor ^^ { case t ~ f => Div (t, f) } |
        (term <~ "MOD") ~ factor ^^ { case t ~ f => Mod (t, f) } |
        (term <~ "&") ~ factor ^^ { case t ~ f => And (t, f) } |
        factor

    lazy val factor : PackratParser[Exp] =
        desig |
        number |
        "(" ~> expression <~ ")" |
        "~" ~> factor ^^ Not |
        "+" ~> factor ^^ Pos |
        "-" ~> factor ^^ Neg

    lazy val desig : PackratParser[Desig] =
        (desig <~ "[") ~ (expression <~ "]") ^^ { case d ~ e => ArrayDesig (d, e) } |
        (desig <~ ".") ~ ident ^^ { case d ~ id => FieldDesig (d, id) } |
        ident

    lazy val number : PackratParser[Literal] =
        integer

    lazy val keyword : PackratParser[String] =
        "ARRAY" | "BEGIN" | "CONST" | "ELSIF" | "ELSE" | "END" | "IF" |
        "MODULE" | "OF" | "PROCEDURE" | "RECORD" | "THEN" | "TYPE" | "VAR" |
        "WHILE"

    lazy val ident : PackratParser[Ident] =
        not (keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r ^^ Ident

    lazy val integer : PackratParser[IntegerLiteral] =
        "[0-9]+".r ^^ (s => IntegerLiteral (s.toInt))

    override protected val whiteSpace =
        """(\s|(\(\*(?:.|[\n\r])*?\*\)))+""".r

    /**
     * Convert an option list into either the list (if present) or Nil if None.
     */
    def optionalListToList[T] (op: Option[List[T]]) : List[T] =
        op.getOrElse (Nil)

}
