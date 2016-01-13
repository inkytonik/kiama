/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2015 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.transform

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Parse the input.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import TransformTree._

    lazy val program =
        rep(opdecl) ~ rep(vardecl) ~ exp ^^ Program

    lazy val opdecl : Parser[(String, Int)] =
        ("op" ~> op) ~ integer

    lazy val op =
        regex("[-!@#$%^&*+_=:;<>,.?]+".r)

    lazy val vardecl =
        "var" ~> ident ^^ VarDecl

    lazy val exp : Parser[ExpR] =
        factor ~ op ~ exp ^^ BinExpR |
            factor ^^ Factor

    lazy val factor =
        integer ^^ Num |
            ident ^^ Var

    lazy val integer =
        "[0-9]+".r ^^ (s => s.toInt)

    lazy val ident =
        regex("[a-zA-Z]+".r)

    override val whitespace : Parser[String] =
        """(\s|(/\*(?:.|[\n\r])*?\*/))*""".r

}
