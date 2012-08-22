/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2012 Anthony M Sloane, Macquarie University.
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

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.kiama
package example.picojava

import org.kiama.util.PositionedParserUtilities

/**
 * PicoJava parser
 */
trait Parser extends PositionedParserUtilities {

    import AbstractSyntax._
    import language.postfixOps

    lazy val parser =
        phrase (program)

    lazy val program =
        block ^^ Program

    lazy val block : PackratParser[Block] =
        "{" ~> (block_stmt*) <~ "}" ^^ Block
    lazy val block_stmt =
        class_decl | var_decl | stmt

    lazy val class_decl =
        "class" ~> IDENTIFIER ~ (xtends?) ~ block ^^ ClassDecl
    lazy val xtends =
        "extends" ~> IDENTIFIER ^^ Use
    lazy val var_decl =
        name ~ IDENTIFIER <~ ";" ^^ VarDecl

    lazy val stmt : PackratParser[Stmt] =
        assign_stmt | while_stmt
    lazy val assign_stmt =
        name ~ ("=" ~> exp <~ ";") ^^ AssignStmt
    lazy val while_stmt =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ WhileStmt

    lazy val exp =
        boolean_literal | name

    lazy val name : PackratParser[Access] =
        name ~ ("." ~> IDENTIFIER) ^^ { case n ~ i => Dot (n, Use (i)) } |
        IDENTIFIER ^^ Use

    lazy val boolean_literal =
        ("true" | "false") ^^ BooleanLiteral

    lazy val IDENTIFIER =
        "[a-zA-Z][a-zA-Z0-9]*".r

    override val whiteSpace =
        """(\s|(//.*\n))+""".r

}
