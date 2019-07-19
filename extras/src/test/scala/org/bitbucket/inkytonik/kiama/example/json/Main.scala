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
package example.json

import JSONTree.{JSONNode, JValue}
import org.bitbucket.inkytonik.kiama.util.Compiler

class Driver extends Compiler[JSONNode, JValue] {

    import PrettyPrinter.{any, pretty}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.parsing.ParseResult
    import org.bitbucket.inkytonik.kiama.util.{Config, Source}

    val name = "json"

    def parse(source : Source, config : Config) : ParseResult[JValue] = {
        val parsers = new SyntaxAnalyser(positions)
        parsers.parseAll(parsers.jvalue, source)
    }

    def process(source : Source, ast : JValue, config : Config) = {

        // Pretty-print tree as a product value
        config.output().emitln(pretty(any(ast)).layout)

        // Pretty-print tree as a JSON value
        config.output().emitln(format(ast).layout)

    }

    def format(ast : JValue) : Document =
        PrettyPrinter.format(ast)

}

object Main extends Driver
