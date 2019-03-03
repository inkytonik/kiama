/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.minijava

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for MiniJava.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import MiniJavaTree._
    import scala.language.postfixOps

    lazy val program =
        mainClass ~ (classDeclaration*) ^^ Program

    lazy val mainClass =
        ("class" ~> idndef) ~ ("{" ~> mainMethod <~ "}") ^^ MainClass

    lazy val mainMethod =
        "public" ~> "static" ~> "void" ~> "main" ~> "(" ~> ")" ~>
            ("{" ~> statement <~ "}") ^^ MainMethod

    lazy val classDeclaration =
        ("class" ~> idndef) ~ (("extends" ~> idnuse)?) ~
            ("{" ~> (fieldDeclaration*)) ~ ((methodDeclaration*) <~ "}") ^^ {
                case n ~ oe ~ vds ~ mds =>
                    Class(n, oe, ClassBody(vds, mds))
            }

    lazy val fieldDeclaration =
        tipe ~ idndef <~ ";" ^^ Field

    lazy val methodDeclaration =
        ("public" ~> tipe) ~ idndef ~ ("(" ~> arguments <~ ")") ~
            ("{" ~> (varDeclaration*)) ~ (statement*) ~ (result <~ "}") ^^ {
                case t ~ n ~ as ~ vds ~ ss ~ rs =>
                    Method(n, MethodBody(t, as, vds, ss, rs))
            }

    lazy val varDeclaration =
        tipe ~ idndef <~ ";" ^^ Var

    lazy val result =
        "return" ~> expression <~ ";" ^^ Result

    lazy val arguments =
        repsep(argument, ",")

    lazy val argument =
        tipe ~ idndef ^^ Argument

    lazy val tipe =
        "int" ~ "[" ~ "]" ^^ (_ => IntArrayType()) |
            "int" ^^ (_ => IntType()) |
            "boolean" ^^ (_ => BooleanType()) |
            not("return") ~> idnuse ^^ ClassType

    lazy val statement : Parser[Statement] =
        "{" ~> (statement*) <~ "}" ^^ Block |
            "if" ~> ("(" ~> expression <~ ")") ~ statement ~ ("else" ~> statement) ^^ If |
            "while" ~> ("(" ~> expression <~ ")") ~ statement ^^ While |
            "System.out.println" ~> ("(" ~> expression <~ ")") <~ ";" ^^ Println |
            idnuse ~ ("=" ~> expression) <~ ";" ^^ VarAssign |
            idnuse ~ ("[" ~> expression <~ "]") ~ ("=" ~> expression) <~ ";" ^^ ArrayAssign

    lazy val expression : PackratParser[Expression] =
        expression ~ ("&&" ~> expression2) ^^ AndExp |
            expression2

    lazy val expression2 : PackratParser[Expression] =
        expression1 ~ ("<" ~> expression1) ^^ LessExp |
            expression1

    lazy val expression1 : PackratParser[Expression] =
        expression1 ~ ("+" ~> expression0) ^^ PlusExp |
            expression1 ~ ("-" ~> expression0) ^^ MinusExp |
            expression0

    lazy val expression0 : PackratParser[Expression] =
        expression0 ~ ("*" ~> factor) ^^ StarExp |
            factor

    lazy val factor : PackratParser[Expression] =
        factor ~ ("[" ~> expression <~ "]") ^^ IndExp |
            factor <~ "." <~ "length" ^^ LengthExp |
            factor ~ ("." ~> idnuse) ~ ("(" ~> expressionList <~ ")") ^^ CallExp |
            integer ^^ {
                case s =>
                    IntExp(s.toInt)
            } |
            "true" ^^ (_ => TrueExp()) |
            "false" ^^ (_ => FalseExp()) |
            "this" ^^ (_ => ThisExp()) |
            "new" ~> "int" ~> "[" ~> expression <~ "]" ^^ NewArrayExp |
            "new" ~> idnuse <~ "(" <~ ")" ^^ NewExp |
            idnuse ^^ IdnExp |
            "!" ~> expression ^^ NotExp |
            "(" ~> expression <~ ")"

    lazy val expressionList =
        repsep(expression, ",")

    lazy val integer =
        regex("[0-9]+".r)

    lazy val idndef =
        identifier ^^ IdnDef

    lazy val idnuse =
        identifier ^^ IdnUse

    lazy val identifier =
        regex("[a-zA-Z][a-zA-Z0-9_]*".r)

    override val whitespace : Parser[String] =
        """(\s|(//.*\n))*""".r

}
