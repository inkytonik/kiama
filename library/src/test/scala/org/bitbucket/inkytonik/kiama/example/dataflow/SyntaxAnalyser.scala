/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2016 Anthony M Sloane, Macquarie University.
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
package example.dataflow

import org.bitbucket.inkytonik.kiama.parsing.ListParsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Syntax analyser for simple imperative dataflow language.
 */
class SyntaxAnalyser(positions : Positions) extends ListParsers(positions) {

    import DataflowTree._
    import scala.language.postfixOps

    lazy val stm : Parser[Stm] =
        asgnStm | whileStm | ifStm | blockStm | returnStm

    lazy val asgnStm =
        idn ~ ("=" ~> exp) ^^ Assign

    lazy val whileStm =
        ("while" ~> "(" ~> exp <~ ")") ~ stm ^^ While

    lazy val ifStm =
        ("if" ~> "(" ~> exp <~ ")") ~ stm ~ ("else" ~> stm) ^^ If

    lazy val blockStm =
        "{" ~> (stm*) <~ "}" ^^ Block

    lazy val returnStm =
        "return" ~> exp ^^ Return

    lazy val exp =
        idn

    lazy val idn =
        not(keyword) ~> "[a-zA-Z]+".r

    lazy val keyword =
        keywords("[^a-zA-Z]".r, List("if", "return", "while"))

}
