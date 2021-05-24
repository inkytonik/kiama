/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
        rep(opdecl) ~ rep(vardecl) ~ exp ^^ Program.apply

    lazy val opdecl : Parser[(String, Int)] =
        ("op" ~> op) ~ integer

    lazy val op =
        regex("[-!@#$%^&*+_=:;<>,.?]+".r)

    lazy val vardecl =
        "var" ~> ident ^^ VarDecl.apply

    lazy val exp : Parser[ExpR] =
        factor ~ op ~ exp ^^ BinExpR.apply |
            factor ^^ Factor.apply

    lazy val factor =
        integer ^^ Num.apply |
            ident ^^ Var.apply

    lazy val integer =
        "[0-9]+".r ^^ (s => s.toInt)

    lazy val ident =
        regex("[a-zA-Z]+".r)

    override val whitespace : Parser[String] =
        """(\s|(/\*(?:.|[\n\r])*?\*/))*""".r

}
