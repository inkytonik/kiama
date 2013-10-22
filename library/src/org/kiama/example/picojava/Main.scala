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
import org.kiama.util.{CompilerWithConfig, Config, Emitter}

/**
 * Configuration for the Picojava compiler.
 */
class PicojavaConfig (args : Array[String], emitter : Emitter) extends Config (args, emitter) {
    val obfuscate = opt[Boolean] ("obfuscate", descr = "Obfuscate the code")
}

object Main extends CompilerWithConfig[Program,PicojavaConfig] with Parser {

    import Obfuscate.obfuscate
    import ErrorCheck.errors
    import PrettyPrinter.pretty
    import org.kiama.util.Config

    def createConfig (args : Array[String], emitter : Emitter = new Emitter) : PicojavaConfig =
        new PicojavaConfig (args, emitter)

    /**
     * Process a picoJava program by checking for errors, optionally obfuscating and
     * then printing any errors that were found.
     */
    override def process (filename : String, program : Program, config : PicojavaConfig) {

        super.process (filename, program, config)

        program->errors

        if (config.obfuscate ()) {
            config.emitter.emitln (pretty (program))
            config.emitter.emitln (pretty (obfuscate (program)))
        }

    }

}
