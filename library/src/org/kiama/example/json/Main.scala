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

package org.kiama
package example.json

import JSONTree.JValue
import org.kiama.util.Compiler

class Driver extends Compiler[JValue] {

    import PrettyPrinter.{any, pretty}
    import org.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}
    import org.kiama.util.{Config, Source}

    val parsers = new SyntaxAnalyser (positions)
    val parser = parsers.jvalue

    def process (source : Source, ast : JValue, config : Config) = {

        // Pretty-print tree as a product value
        config.output.emitln (pretty (any (ast)))

        // Pretty-print tree as a JSON value
        config.output.emitln (format (ast))

    }

    def format (ast : JValue) : Document =
        emptyDocument

}

object Main extends Driver
