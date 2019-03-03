/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
