/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package example.picojava

import AbstractSyntax.Program
import org.kiama.util.ProfilingCompiler

object Main extends ProfilingCompiler[Program] with Parser {

    import Obfuscate.obfuscate
    import ErrorCheck.errors
    import PrettyPrinter.pretty
    import org.kiama.util.Console
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._

    /**
     * Whether or not to perform program obfuscation.
     */
    var doObfuscation = false

    /**
     * Check for `-o` which means print out the program and its obfuscated form.
     */
    override def checkargs (args : Array[String], emitter : Emitter) : Array[String] =
        if (args.size > 0 && args (0) == "-o") {
            doObfuscation = true
            args.drop (1)
        } else
            args

    /**
     * Process a picoJava program by checking for errors, optionally obfuscating and
     * then printing any errors that were found.
     */
    override def process (filename : String, program : Program, console : Console, emitter : Emitter) : Boolean = {

        super.process (filename, program, console, emitter)

        resetmessages
        program->errors

        if (doObfuscation) {
            emitter.emitln (pretty (program))
            emitter.emitln (pretty (obfuscate (program)))
        }

        if (messagecount > 0) {
            report (emitter)
            false
        } else
            true

    }

}
