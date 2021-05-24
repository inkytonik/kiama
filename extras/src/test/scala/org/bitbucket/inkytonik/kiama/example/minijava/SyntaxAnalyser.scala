/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2021 Anthony M Sloane, Macquarie University.
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

    lazy val program =
        mainClass ~ rep(classDeclaration) ^^ Program.apply

    lazy val mainClass =
        ("class" ~> idndef) ~ ("{" ~> mainMethod <~ "}") ^^ MainClass.apply

    lazy val mainMethod =
        "public" ~> "static" ~> "void" ~> "main" ~> "(" ~> ")" ~>
            ("{" ~> statement <~ "}") ^^ MainMethod.apply

    lazy val classDeclaration =
        ("class" ~> idndef) ~ opt("extends" ~> idnuse) ~
            ("{" ~> rep(fieldDeclaration)) ~ (rep(methodDeclaration) <~ "}") ^^ {
                case n ~ oe ~ vds ~ mds =>
                    Class(n, oe, ClassBody(vds, mds))
            }

    lazy val fieldDeclaration =
        tipe ~ idndef <~ ";" ^^ Field.apply

    lazy val methodDeclaration =
        ("public" ~> tipe) ~ idndef ~ ("(" ~> arguments <~ ")") ~
            ("{" ~> rep(varDeclaration)) ~ rep(statement) ~ (result <~ "}") ^^ {
                case t ~ n ~ as ~ vds ~ ss ~ rs =>
                    Method(n, MethodBody(t, as, vds, ss, rs))
            }

    lazy val varDeclaration =
        tipe ~ idndef <~ ";" ^^ Var.apply

    lazy val result =
        "return" ~> expression <~ ";" ^^ Result.apply

    lazy val arguments =
        repsep(argument, ",")

    lazy val argument =
        tipe ~ idndef ^^ Argument.apply

    lazy val tipe =
        "int" ~ "[" ~ "]" ^^ (_ => IntArrayType()) |
            "int" ^^ (_ => IntType()) |
            "boolean" ^^ (_ => BooleanType()) |
            not("return") ~> idnuse ^^ ClassType.apply

    lazy val statement : Parser[Statement] =
        "{" ~> rep(statement) <~ "}" ^^ Block.apply |
            "if" ~> ("(" ~> expression <~ ")") ~ statement ~ ("else" ~> statement) ^^ If.apply |
            "while" ~> ("(" ~> expression <~ ")") ~ statement ^^ While.apply |
            "System.out.println" ~> ("(" ~> expression <~ ")") <~ ";" ^^ Println.apply |
            idnuse ~ ("=" ~> expression) <~ ";" ^^ VarAssign.apply |
            idnexp ~ ("[" ~> expression <~ "]") ~ ("=" ~> expression) <~ ";" ^^ ArrayAssign.apply

    lazy val expression : PackratParser[Expression] =
        expression ~ ("&&" ~> expression2) ^^ AndExp.apply |
            expression2

    lazy val expression2 : PackratParser[Expression] =
        expression1 ~ ("<" ~> expression1) ^^ LessExp.apply |
            expression1

    lazy val expression1 : PackratParser[Expression] =
        expression1 ~ ("+" ~> expression0) ^^ PlusExp.apply |
            expression1 ~ ("-" ~> expression0) ^^ MinusExp.apply |
            expression0

    lazy val expression0 : PackratParser[Expression] =
        expression0 ~ ("*" ~> factor) ^^ StarExp.apply |
            factor

    lazy val factor : PackratParser[Expression] =
        factor ~ ("[" ~> expression <~ "]") ^^ IndExp.apply |
            factor <~ "." <~ "length" ^^ LengthExp.apply |
            factor ~ ("." ~> idnuse) ~ ("(" ~> expressionList <~ ")") ^^ CallExp.apply |
            integer ^^ {
                case s =>
                    IntExp(s.toInt)
            } |
            "true" ^^ (_ => TrueExp()) |
            "false" ^^ (_ => FalseExp()) |
            "this" ^^ (_ => ThisExp()) |
            "new" ~> "int" ~> "[" ~> expression <~ "]" ^^ NewArrayExp.apply |
            "new" ~> idnuse <~ "(" <~ ")" ^^ NewExp.apply |
            idnexp |
            "!" ~> expression ^^ NotExp.apply |
            "(" ~> expression <~ ")"

    lazy val idnexp : PackratParser[IdnExp] =
        idnuse ^^ IdnExp.apply

    lazy val expressionList =
        repsep(expression, ",")

    lazy val integer =
        regex("[0-9]+".r)

    lazy val idndef =
        identifier ^^ IdnDef.apply

    lazy val idnuse =
        identifier ^^ IdnUse.apply

    lazy val identifier =
        regex("[a-zA-Z][a-zA-Z0-9_]*".r)

    override val whitespace : Parser[String] =
        """(\s|(//.*\n))*""".r

}
