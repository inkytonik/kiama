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
package example.dataflow

import org.bitbucket.inkytonik.kiama.parsing.ListParsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Syntax analyser for simple imperative dataflow language.
 */
class SyntaxAnalyser(positions : Positions) extends ListParsers(positions) {

    import DataflowTree._

    lazy val stm : Parser[Stm] =
        asgnStm | whileStm | ifStm | blockStm | returnStm

    lazy val asgnStm =
        idn ~ ("=" ~> exp) ^^ Assign.apply

    lazy val whileStm =
        ("while" ~> "(" ~> exp <~ ")") ~ stm ^^ While.apply

    lazy val ifStm =
        ("if" ~> "(" ~> exp <~ ")") ~ stm ~ ("else" ~> stm) ^^ If.apply

    lazy val blockStm =
        "{" ~> rep(stm) <~ "}" ^^ Block.apply

    lazy val returnStm =
        "return" ~> exp ^^ Return.apply

    lazy val exp =
        idn

    lazy val idn =
        not(keyword) ~> "[a-zA-Z]+".r

    lazy val keyword =
        keywords("[^a-zA-Z]".r, List("if", "return", "while"))

}
