/**
 * JSON example main program.
 *
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
package example.json

import JSONTree.JValue
import org.kiama.util.Compiler

object Main extends Driver

class Driver extends SyntaxAnalysis with Compiler[JValue] {

    import PrettyPrinter.{pretty_any, pretty}
    import org.kiama.util.Console
    import org.kiama.util.Emitter

    /**
     * The usage message for an erroneous invocation.
     */
    val usage = "usage: run file.json"

    /**
     * Process the tree (currently just print it).
     */
    override def process (ast : JValue, console : Console, emitter : Emitter) : Boolean = {

        super.process (ast, console, emitter)

        // Pretty-print AST as a product value
        emitter.emitln (pretty_any (ast))

        // Pretty-print AST as a JSON value
        emitter.emitln (pretty (ast))

        true

    }

}
