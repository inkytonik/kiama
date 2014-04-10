/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package util

import org.rogach.scallop.ScallopConf
import scala.collection.immutable.Seq

/**
 * Configurations for Kiama programs. `args` gives the command-line
 * arguments that are used to determine many of the configuration
 * settings. The emitters allow the output and errors targets to be
 * altered for testing. `output` defaults to

 `emitter` allows the output target to be altered for
 * purposes such as testing; it defaults to standard output.
 */
class Config (args : Seq[String], val output : Emitter, val error : Emitter) extends ScallopConf (args) {

    import org.kiama.util.{FileConsole, JLineConsole, StringConsole}
    import org.rogach.scallop.{ArgType, ValueConverter}
    import scala.reflect.runtime.universe.TypeTag

    /**
     * Convertor for console options.
     */
    val consoleConverter =
        new ValueConverter[Console] {

            val argType = ArgType.LIST

            def parse (s : List[(String, List[String])]) : Either[String,Option[Console]] =
                s match {
                    case List ((_, List ("file", filename))) =>
                        Right (Some (new FileConsole (filename)))
                    case List ((_, List ("string", contents))) =>
                        Right (Some (new StringConsole (contents)))
                    case List ((_, _)) =>
                        Left ("expected 'file name' or 'string value'")
                    case _ =>
                        Right (None)
                }

            val tag = implicitly[TypeTag[Console]]

        }

    /**
     * The console to use for reading input. Defaults to a JLine console.
     * Options are a string console where the option value specifies the
     * contents, and a file console where the option value specifies the
     * file name.
     */
    val console = opt[Console] ("console", descr = "Console for program input",
                                default = Some (JLineConsole)) (consoleConverter)

    /**
     * Profiling dimensions.
     */
    val profile = opt[String] ("profile", descr = "Profiling dimensions (comma-separated)")

    /**
     * Logging option. If profiling and this is set, print out events as they are generated.
     */
    val logging = opt[Boolean] ("logging", descr = "Print profile events dynamically)",
                                default = Some (false))

    /**
     * Time option. If set, print out execution time report.
     */
    val time = opt[Boolean] ("time", descr = "Report execution time")

    /**
     * The zero or more filenames that were specified positionally after all of the options.
     */
    val filenames = trailArg[List[String]] ("files", descr = "Input files",
                                            required = false,
                                            default = Some (List ()))

    /**
     * Handle errors by printing them, then printing the help message, then
     * exiting. All output is performed to the errors emitter.
     */
    errorMessageHandler =
        (message : String) => {
            error.emitln (s"Command-line error: $message")
            error.emitln (builder.help)
            sys.exit (1)
        }

}

/**
 * Configurations for Kiama REPLS. Adds some options to the default
 * set that all Kiama programs support.
 */
class REPLConfig (args : Seq[String], output : Emitter, error : Emitter) extends Config (args, output, error) {

    /**
     * Whitespace option. If set, pass input lines that are completely white space
     * to the REPL processing. By default, these lines are ignored.
     */
    val processWhitespaceLines = opt[Boolean] ("processWhitespaceLines", 'w', descr = "Process whitespace lines")

}
