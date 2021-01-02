/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.json

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for the JSON language.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import JSONTree._

    lazy val jvalue : Parser[JValue] =
        jobject | jarray | jstring | jnumber | jtrue | jfalse | jnull

    lazy val jobject =
        "{" ~> repsep(jpair, ",") <~ "}" ^^ JObject

    lazy val jpair =
        jname ~ (":" ~> jvalue) ^^ {
            case n ~ v => (n, v)
        }

    lazy val jname =
        string ^^ JName

    lazy val jarray =
        "[" ~> repsep(jvalue, ",") <~ "]" ^^ {
            case l => JArray(l)
        }

    lazy val jstring =
        string ^^ JString

    lazy val string =
        regex("\"[^\"]*\"".r) ^^ {
            case s => s.substring(1, s.length - 1)
        }

    lazy val jnumber =
        regex("""-?(0|[1-9]\d*)(\.\d+)?([eE][-+]?\d+)?""".r) ^^ {
            case s => JNumber(s.toDouble)
        }

    lazy val jtrue =
        "true" ^^ (_ => JTrue())

    lazy val jfalse =
        "false" ^^ (_ => JFalse())

    lazy val jnull =
        "null" ^^ (_ => JNull())

}
