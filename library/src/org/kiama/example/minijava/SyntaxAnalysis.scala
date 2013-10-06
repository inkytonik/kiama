/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2013 Anthony M Sloane, Macquarie University.
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
package example.minijava

import org.kiama.util.PositionedParserUtilities

/**
 * Module containing parsers for MiniJava.
 */
class SyntaxAnalysis extends PositionedParserUtilities {

    import MiniJavaTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        mainClass ~ (classDeclaration*) ^^ Program

    lazy val mainClass : PackratParser[MainClass] =
        (("class" ~> idndef) <~
            ("{" <~ "public" <~ "static" <~ "void" <~ "main" <~ "(" <~ ")" <~ "{")) ~
                (statement <~ "}" <~ "}") ^^ MainClass

    lazy val classDeclaration : PackratParser[Class] =
        ("class" ~> idndef) ~ (("extends" ~> idnuse)?) ~
            ("{" ~> (fieldDeclaration*)) ~ ((methodDeclaration*) <~ "}") ^^ {
                case n ~ oe ~ vds ~ mds =>
                    Class (n, oe, ClassBody (vds, mds))
            }

    lazy val fieldDeclaration : PackratParser[Field] =
        tipe ~ idndef <~ ";" ^^ Field

    lazy val methodDeclaration : PackratParser[Method] =
        ("public" ~> tipe) ~ idndef ~ ("(" ~> arguments <~ ")") ~
            ("{" ~> (varDeclaration*)) ~ (statement*) ~ (result <~ "}") ^^ {
                case t ~ n ~ as ~ vds ~ ss ~ rs =>
                    Method (n, MethodBody (t, as, vds, ss, rs))
            }

    lazy val varDeclaration : PackratParser[Var] =
        tipe ~ idndef <~ ";" ^^ Var

    lazy val result : PackratParser[Expression] =
        "return" ~> expression <~ ";"

    lazy val arguments : PackratParser[List[Argument]] =
        repsep (argument, ",")

    lazy val argument : PackratParser[Argument] =
        tipe ~ idndef ^^ Argument

    lazy val tipe : PackratParser[Type] =
        "int" ~ "[" ~ "]" ^^ (_ => IntArrayType ()) |
        "int" ^^ (_ => IntType ()) |
        "boolean" ^^ (_ => BooleanType ()) |
        not ("return") ~> idnuse ^^ ClassType

    lazy val statement : PackratParser[Statement] =
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
                IntExp (s.toInt)
        } |
        "true" ^^ (_ => TrueExp ()) |
        "false" ^^ (_ => FalseExp ()) |
        "this" ^^ (_ => ThisExp ()) |
        "new" ~> "int" ~> "[" ~> expression <~ "]" ^^ NewArrayExp |
        "new" ~> idnuse <~ "(" <~ ")" ^^ NewExp |
        idnuse ^^ IdnExp |
        "!" ~> expression ^^ NotExp |
        "(" ~> expression <~ ")"

    lazy val expressionList : PackratParser[List[Expression]] =
        repsep (expression, ",")

    lazy val integer : PackratParser[String] =
        regex ("[0-9]+".r)

    lazy val idndef : PackratParser[IdnDef] =
        identifier ^^ IdnDef

    lazy val idnuse : PackratParser[IdnUse] =
        identifier ^^ IdnUse

    lazy val identifier : PackratParser[String] =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r)

    override val whiteSpace =
        """(\s|(//.*\n))+""".r

}
