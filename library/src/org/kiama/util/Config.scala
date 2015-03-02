/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2015 Anthony M Sloane, Macquarie University.
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
 * settings.
 */
abstract class Config (args : Seq[String]) extends ScallopConf (args) {

    import org.kiama.util.{FileConsole, JLineConsole, StringConsole}
    import org.rogach.scallop.{ArgType, ValueConverter}
    import scala.reflect.runtime.universe.TypeTag

    /**
     * The emitter to use for normal output.
     */
    def output : Emitter

    /**
     * The emitter to use for error output.
     */
    def error : Emitter

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
    lazy val console = opt[Console] ("Kconsole", descr = "Console for program input",
                                     default = Some (JLineConsole),
                                     noshort = true,
                                     hidden = true) (consoleConverter)

    /**
     * Profiling dimensions.
     */
    lazy val profile = opt[String] ("Kprofile",
                                    descr = "Profiling dimensions (comma-separated)",
                                    noshort = true,
                                    hidden = true)

    /**
     * Logging option. If profiling and this is set, print out events as they are generated.
     */
    lazy val logging = toggle ("Klogging",
                               descrYes = "Print profile events dynamically",
                               descrNo = "Don't print profile events",
                               default = Some (false),
                               noshort = true,
                               hidden = true)

    /**
     * Time option. If set, print out execution time report.
     */
    lazy val time = toggle ("Ktime",
                            descrYes = "Report execution time",
                            descrNo = "Don't report execution time",
                            default = Some (false),
                            noshort = true,
                            hidden = true)

    /**
     * The zero or more filenames that were specified positionally after all of the options.
     */
    lazy val filenames = trailArg[List[String]] ("files", descr = "Input files",
                                                 required = false,
                                                 default = Some (Nil))

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
abstract class REPLConfig (args : Seq[String]) extends Config (args) {

    /**
     * Whitespace option. If set, pass input lines that are completely white space
     * to the REPL processing. By default, these lines are ignored.
     */
    lazy val processWhitespaceLines = toggle ("KprocessWhitespaceLines",
                                              descrYes = "Process whitespace lines",
                                              descrNo = "Don't process whitespace lines",
                                              default = Some (false),
                                              hidden = true)

}
