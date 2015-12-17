/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2015 Anthony M Sloane, Macquarie University.
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
package example.json

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for the JSON language.
 */
class SyntaxAnalyser (positions : Positions) extends Parsers (positions) {

    import JSONTree._

    lazy val jvalue : Parser[JValue] =
        jobject | jarray | jstring | jnumber | jtrue | jfalse | jnull

    lazy val jobject =
        "{" ~> repsep (jpair, ",") <~ "}" ^^ JObject

    lazy val jpair =
        jname ~ (":" ~> jvalue) ^^ {
            case n ~ v => (n, v)
        }

    lazy val jname =
        string ^^ JName

    lazy val jarray =
        "[" ~> repsep (jvalue, ",") <~ "]" ^^ {
            case l => JArray (l)
        }

    lazy val jstring =
        string ^^ JString

    lazy val string =
        regex ("\"[^\"]*\"".r) ^^ {
            case s => s.substring (1, s.length - 1)
        }

    lazy val jnumber =
        regex ("""-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?""".r) ^^ {
            case s => JNumber (s.toDouble)
        }

    lazy val jtrue =
        "true" ^^ (_ => JTrue ())

    lazy val jfalse =
        "false" ^^ (_ => JFalse ())

    lazy val jnull =
        "null" ^^ (_ => JNull ())

}
