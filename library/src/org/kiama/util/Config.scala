/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013 Anthony M Sloane, Macquarie University.
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

/**
 * Configurations for Kiama programs. `args` gives the command-line
 * arguments that are used to determine many of the configuration
 * settings. `emitter` allows the output target to be altered for
 * purposes such as testing; it defaults to standard output.
 */
class Config (args : Array[String], val emitter : Emitter) extends ScallopConf (args) {

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
     * Time option. If set, print out execution time report.
     */
    val time = opt[Boolean] ("time", descr = "Report execution time")

    /**
     * The filenames that were specified positionally after all of the options.
     */
    val filenames = trailArg[List[String]] ("files", descr = "Input files")

    /**
     * Handle errors by printing them, then printing the help message, then
     * exiting. All output is performed to the emitter.
     */
    errorMessageHandler =
        (message : String) => {
            emitter.emitln (s"Command-line error: $message")
            emitter.emitln (builder.help)
            sys.exit (1)
        }

}
