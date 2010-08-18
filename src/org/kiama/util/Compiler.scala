/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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

package org.kiama.util

import java.io.FileReader
import org.scalatest.FunSuite
import scala.util.parsing.combinator.RegexParsers

/**
 * Trait to provide basic functionality for a compiler-like program
 * constructed from phases.
 */
trait Compiler[T] extends FunSuite {

    import java.io.File
    import java.io.FileNotFoundException
    import org.kiama.util.Console
    import org.kiama.util.JLineConsole
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging._
    import org.kiama.util.StringEmitter
    import org.scalatest.TestFailedException
    import scala.io.Source

    /**
     * Process the program in the file given as the first command-line
     * argument, read input using JLine input editing, and emit output
     * to the standard output.
     */
    def main (args : Array[String]) {
        driver (args, JLineConsole, new Emitter)
    }

    /**
     * Process the command-line arguments.  Returns the arguments that
     * have not been processed.  Default: do no processing.
     */
    def checkargs (args : Array[String]) : Array[String] =
        args

    /**
     * Process the program in the file given as the first command-line
     * argument by building an AST for it and then processing that AST
     * in an arbitrary way.  The AST is built by makeast and processing
     * is performed by process.  Output is produced using the specified
     * emitter.  True is returned if it all worked, otherwise false.
     */
    def driver (args : Array[String], console : Console, emitter : Emitter) : Boolean = {
        val newargs = checkargs (args)
        newargs.size match {
            case 1 =>
                try {
                    val reader = new FileReader (newargs (0))
                    makeast (reader, newargs (0)) match {
                        case Some (ast) =>
                            process (ast, console, emitter)
                        case None =>
                            false
                    }
                } catch {
                    case e : FileNotFoundException =>
                        println (e.getMessage)
                        false
                }
            case _ =>
                println (usage)
                false
        }
    }

    /**
     * The usage message for an erroneous invocation.
     */
    val usage : String

    /**
     * Make an AST from the file with the given name.  Returns None
     * if an AST cannot be made.
     */
    def makeast (reader : FileReader, filename : String) : Option[T]

    /**
     * Function to process the input that was parsed.  console should be
     * used to read anything needed by the processing.  emitter should be
     * used for output.  Return true if everything worked, false otherwise.
     */
    def process (ast : T, console : Console, emitter : Emitter) : Boolean

    /**
     * Run the driver using the given args and return the resulting output
     * or None if the driver failed.  Read standard input from the specified
     * console.  Reset the message buffer before calling the driver.
     */
    def compile (args : Array[String], console : Console) : Option[String] = {
        val emitter = new StringEmitter
        Messaging.resetmessages
        if (driver (args, console, emitter)) {
            Some (emitter.result ())
        } else {
            None
        }
    }

    /**
     * Make a single file test processing the file cp with comamnd-line
     * arguments args, expecting output as in the file rp.  Use the given
     * console for input.  The extra string is used is appended to the
     * normal test title. name is an identifying string used in messages.
     * If the compilation fails, rp is assumed to contain the expected
     * messages.
     */
    private def filetest (name : String, cp : String, rp : String,
                          console : Console,
                  extra : String = "",
                  args : Array[String] = Array()) {
        val title = name + " " + args.mkString(" ") + " processing " + cp +
                    " expecting " + rp + extra
        test (title) {
            val res =
                try {
                    compile (args :+ cp, console)
                } catch {
                    case e : Exception =>
                        info ("failed with an exception ")
                        throw (e)
                }
            val rc = Source.fromFile (rp).mkString
            res match {
                case Some (cc) =>
                    if (cc != rc)
                        fail (title + " generated bad output:\n" + cc +
                             "expected:\n" + rc)
                case None =>
                    val ec = Messaging.toString
                    if (ec != rc)
                        fail (cp + " compilation failed with wrong errors:\n" + ec +
                              "expected:\n" + rc)
            }
        }
    }

    /**
     * Make tests that process the files in path.  name is an identifying
     * name for this set of tests.  All files whose names end in srcext are
     * processed.  Processing is done by the function compile which must
     * return either Some (s) where s is the output or None if processing
     * failed.  If srcext is .x and resext is .y, then the expected result
     * for foo.x is found in file foo.y.  If optinext is Some (z), then
     * foo.z is used for standard input.  A test fails if either the
     * processing fails or it succeeds with the wrong result.  argslist
     * is used to specify the sets of command-line arguments that you want
     * to use.  Each test is run with each set of arguments.  The default
     * is an empty argument list.
     */
    def filetests (name : String, path : String, srcext : String,
                   resext : String, optinext : Option[String] = None,
                   argslist : List[Array[String]] = List (Array ())) {

        import java.io.FilenameFilter

        /**
         * Make a set of file tests from using inputs from dir with inputs
         * selected by infilter with extensions inext.  If there are no
         * such input files, then no tests are created.  name is an identifying
         * string used in messages.
         */
        def infiletests (name : String, cp : String, dir : File,
                         infilter : FilenameFilter, inext : String,
                         args : Array[String]) {
            for (i <- dir.list (infilter)) {
                val ip = path + "/" + i
                val console = new FileConsole (ip)
                val rp = ip.replace (inext, resext)
                filetest (name, cp, rp, console, " from input " + ip, args)
            }
        }

        val dir = new File (path)
        val children = dir.list ()
        if (children == null) {
            test ("Run file tests at " + path) {
                fail ("bad test file path " + path)
            }
        } else {
            for (args <- argslist) {
                for (c <- children) {
                    if (c.endsWith (srcext)) {
                        val cp = path + "/" + c
                        optinext match {
                            case Some (inext) =>
                                val infilter =
                                    new FilenameFilter {
                                        def accept (dir : File, name : String) : Boolean = {
                                            name.startsWith (c) && name.endsWith (inext)
                                        }
                                    }
                                infiletests (name, cp, dir, infilter, inext, args)
                            case None =>
                                val rp = cp.replace (srcext, resext)
                                filetest (name, cp, rp, null, "", args)
                        }
                    }
                }
            }
        }

    }

}

/**
 * A compiler that uses a Scala combinator character-level parser.
 */
trait RegexCompiler[T] extends Compiler[T] {

    this : RegexParsers =>

    /**
     * The actual parser used to produce the AST.
     */
    val parser : Parser[T]

    /**
     * Make an AST from the file with the given name.  Actually calls
     * parser to do the real work and checks its output.  Return None
     * and print errors if the parse fails.
     */
    def makeast (reader : FileReader, filename : String) : Option[T] = {
        parseAll (parser, reader) match {
            case Success (ast, _) =>
                Some (ast)
            case f =>
                println (f)
                None
        }
    }

}