/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2016 Anthony M Sloane, Macquarie University.
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
package example.prolog

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for Prolog.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import PrologTree._
    import scala.language.postfixOps

    lazy val program =
        (clause+) ^^ Program

    lazy val query =
        lit <~ "."

    lazy val clause =
        lit ~ (":-" ~> lits) <~ "." ^^ Rule |
            lit <~ "." ^^ Fact

    lazy val lit : Parser[Literal] =
        atom ~ ("(" ~> terms <~ ")") ^^ Pred |
            atom ^^ Atom

    lazy val lits =
        rep1sep(lit | cut, ",")

    lazy val cut =
        "!" ^^ { case _ => Cut() }

    lazy val terms =
        rep1sep(term, ",")

    lazy val term =
        lit |
            varr ^^ Var |
            integer |
            list

    lazy val list =
        "[" ~> "]" ^^ { case _ => Pred("nil", Vector()) } |
            "[" ~> listterms <~ "]"

    lazy val listterms : Parser[Literal] =
        term ~ ("," ~> listterms) ^^ {
            case h ~ t => Pred("cons", Vector(h, t))
        } |
            term ^^ {
                case h => Pred("cons", Vector(h, Pred("nil", Vector())))
            }

    lazy val atom =
        regex("[a-z][a-zA-Z]*".r)

    lazy val varr =
        regex("[A-Z][a-zA-Z]*".r)

    lazy val integer =
        regex("[0-9]+".r) ^^ { case s => Integer(s.toInt) }

}
