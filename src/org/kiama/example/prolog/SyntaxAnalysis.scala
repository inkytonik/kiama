/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package example.prolog

import org.kiama.util.PositionedParserUtilities

/**
 * Module containing parsers for Prolog.
 */
class SyntaxAnalysis extends PositionedParserUtilities {

    import PrologTree._

    lazy val parser =
        phrase (program)

    lazy val program =
        (clause+) ^^ Program

    lazy val query =
        literal <~ "."

    lazy val clause =
        literal ~ (":-" ~> literals) <~ "." ^^ Rule |
        literal <~ "." ^^ Fact

    lazy val literal : PackratParser[Literal] =
        atom ~ ("(" ~> terms <~ ")") ^^ Pred |
        atom ^^ Atom

    lazy val literals =
        rep1sep (literal | cut, ",")

    lazy val cut =
        "!" ^^ { case _ => Cut () }

    lazy val terms =
        rep1sep (term, ",")

    lazy val term =
        literal |
        varr ^^ Var |
        integer |
        list

    lazy val list =
        "[" ~> "]" ^^ { case _ => Pred ("nil", Nil) } |
        "[" ~> listterms <~ "]"

    lazy val listterms : PackratParser[Literal] =
        term ~ ("," ~> listterms) ^^ {
            case h ~ t => Pred ("cons", List (h, t))
        } |
        term ^^ {
            case h => Pred ("cons", List (h, Pred ("nil", Nil)))
        }

    lazy val atom =
        regex ("[a-z][a-zA-Z]*".r)

    lazy val varr =
        regex ("[A-Z][a-zA-Z]*".r)

    lazy val integer =
        regex ("[0-9]+".r) ^^ { case s => Integer (s.toInt) }

}
