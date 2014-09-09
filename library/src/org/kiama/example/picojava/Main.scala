/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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

import PicoJavaTree.Program
import org.kiama.util.{CompilerWithConfig, Config, Emitter, ErrorEmitter,
    OutputEmitter}
import scala.collection.immutable.Seq

/**
 * Configuration for the Picojava compiler.
 */
class PicojavaConfig (args : Seq[String], output : Emitter, error : Emitter) extends Config (args, output, error) {
    val obfuscate = opt[Boolean] ("obfuscate", descr = "Obfuscate the code")
}

object Main extends CompilerWithConfig[Program,PicojavaConfig] with SyntaxAnalyser {

    import Obfuscator.obfuscate
    import ErrorCheck.errors
    import PrettyPrinter.pretty
    import org.kiama.util.Config
    import org.kiama.util.Messaging.report

    def createConfig (args : Seq[String],
                      output : Emitter = new OutputEmitter,
                      error : Emitter = new ErrorEmitter) : PicojavaConfig =
        new PicojavaConfig (args, output, error)

    /**
     * Process a picoJava program by checking for errors, printing them if any are
     * found. If there are no errors, optionally obfuscate.
     */
    override def process (filename : String, program : Program, config : PicojavaConfig) {

        super.process (filename, program, config)

        val messages = program->errors
        if (messages.length > 0)
            report (messages)
        else if (config.obfuscate ()) {
            config.output.emitln (pretty (program))
            config.output.emitln (pretty (obfuscate (program)))
        }

    }

}
