/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.bitbucket.inkytonik.kiama
package example.picojava

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * PicoJava parser
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import PicoJavaTree._

    lazy val program =
        block ^^ Program.apply

    lazy val block : Parser[Block] =
        "{" ~> rep(block_stmt) <~ "}" ^^ Block.apply
    lazy val block_stmt =
        class_decl | var_decl | stmt

    lazy val class_decl =
        "class" ~> IDENTIFIER ~ opt(xtends) ~ block ^^ ClassDecl.apply
    lazy val xtends =
        "extends" ~> IDENTIFIER ^^ Use.apply
    lazy val var_decl =
        name ~ IDENTIFIER <~ ";" ^^ VarDecl.apply

    lazy val stmt : Parser[Stmt] =
        assign_stmt | while_stmt
    lazy val assign_stmt =
        name ~ ("=" ~> exp <~ ";") ^^ AssignStmt.apply
    lazy val while_stmt =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ WhileStmt.apply

    lazy val exp =
        boolean_literal | name

    lazy val name : PackratParser[Access] =
        name ~ ("." ~> IDENTIFIER) ^^ { case n ~ i => Dot(n, Use(i)) } |
            IDENTIFIER ^^ Use.apply

    lazy val boolean_literal =
        ("true" | "false") ^^ BooleanLiteral.apply

    lazy val IDENTIFIER =
        "[a-zA-Z][a-zA-Z0-9]*".r

    override val whitespace : Parser[String] =
        """(\s|(//.*\n))*""".r

}
