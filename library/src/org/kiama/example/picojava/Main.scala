/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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

/**
 * Configuration for the PicoJava compiler.
 */
abstract class PicojavaConfig (args : Seq[String]) extends Config (args) {
    lazy val obfuscate = opt[Boolean] ("obfuscate", descr = "Obfuscate the code")
}

object Main extends CompilerWithConfig[Program,PicojavaConfig] with SyntaxAnalyser {

    import PicoJavaTree.PicoJavaTree
    import org.kiama.output.PrettyPrinterTypes.Document
    import org.kiama.util.Config

    def createConfig (args : Seq[String],
                      out : Emitter = new OutputEmitter,
                      err : Emitter = new ErrorEmitter) : PicojavaConfig =
        new PicojavaConfig (args) {
            lazy val output = out
            lazy val error = err
        }

    /**
     * Process a PicoJava program by checking for errors, optionally obfuscating and
     * then printing any errors that were found.
     */
    def process (filename : String, program : Program, config : PicojavaConfig) {

        val tree = new PicoJavaTree (program)
        val analysis = new ErrorCheck (tree)
        val messages = analysis.errors

        if (messages.size > 0) {
            // Note, prints array list, no coords
            config.output.emitln (messages)
        } else if (config.obfuscate ()) {
            val obfuscator = new Obfuscator (analysis)
            config.output.emitln (format (program))
            config.output.emitln (format (obfuscator.obfuscate (program)))
        }

    }

    /**
     * Pretty printer to use to print minijava ASTs.
     */
    override def format (ast : Program) : Document =
        PrettyPrinter.format (ast)

}
