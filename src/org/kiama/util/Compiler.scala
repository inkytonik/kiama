/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2011 Anthony M Sloane, Macquarie University.
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
    import org.kiama.util.StringEmitter
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
     * have not been processed.  Output should be emitted using the
     * provided Emitter.  Default: do no processing.
     */
    def checkargs (args : Array[String], emitter : Emitter) : Array[String] =
        args

    /**
     * Process the arguments, using the given console for input and the
     * given emitter for output.  The arguments are first processed by
     * checkargs.  Any remaining arguments are interpreted as file names
     * which are processed in turn by using makeast to turn their contents
     * into abstract syntax trees (ASTs) and then by process which conducts 
     * arbitrary processing on the ASTs.
     */
    def driver (args : Array[String], console : Console, emitter : Emitter) {
        val newargs = checkargs (args, emitter)
        for (arg <- newargs) {
            try {
                val reader = new FileReader (newargs (0))
                makeast (reader, newargs (0)) match {
                    case Left (ast) =>
                        process (ast, console, emitter)
                    case Right (msg) =>
                        emitter.emitln (msg)
                }
            } catch {
                case e : FileNotFoundException =>
                    emitter.emitln (e.getMessage)
            }
        }
    }

    /**
     * Make an AST from the file with the given name, returning it wrapped in
     * Left.  Returns Right with an error message if an AST cannot be made.
     */
    def makeast (reader : FileReader, filename : String) : Either[T,String]

    /**
     * Function to process the input that was parsed.  console should be
     * used to read anything needed by the processing.  emitter should be
     * used for output.  Return true if everything worked, false otherwise.
     * If false is returned, messages about the problem should be logged
     * by process using the messaging facility.
     */
    def process (ast : T, console : Console, emitter : Emitter) : Boolean

    /**
     * Run the driver using the given args and return the resulting output,
     * which may be error messages or the result of running the compiled
     * program, for example. Read standard input from the specified console.
     * Reset the message buffer before calling the driver.
     */
    def compile (args : Array[String], console : Console) : String = {
        val emitter = new StringEmitter
        Messaging.resetmessages
        driver (args, console, emitter)
        emitter.result
    }

    /**
     * Flag to decide whether to sanitise the output before comparison
     * of test results with expected results (see sanitise method).
     * Default is true; override with false if you want actual results
     * compared.
     */
    def dosanitisation = true

    /**
     * Sanitise the output.  At the moment this means make any Windows line
     * endings appear in Unix style instead.  This allows for either program
     * or test output to use either line ending style, but tests will still
     * pass.  This will clearly break any tests where the actual line endings
     * matter.
     */
    def sanitise (s : String) : String =
        if (dosanitisation)
            s.replaceAll ("\r", "\n")
        else
            s

    /**
     * Make a single file test processing the file cp with comamnd-line
     * arguments args, expecting output as in the file rp.  Use the given
     * console for input.  The extra string is used is appended to the
     * normal test title. name is an identifying string used in messages.
     * If the compilation fails, rp is assumed to contain the expected
     * messages.
     */
    def filetest (name : String, cp : String, rp : String,
                  console : Console, extra : String = "",
                  args : Array[String] = Array()) {
        val title = name + " " + args.mkString ("(", " ", ")") + " processing " + cp +
                    " expecting " + rp + extra
        test (title) {
            val cc =
                try {
                    compile (args :+ cp, console)
                } catch {
                    case e : Exception =>
                        info ("failed with an exception ")
                        throw (e)
                }
            val rc = Source.fromFile (rp).mkString
            if (sanitise (cc) != sanitise (rc))
                fail (title + " generated bad output:\n" + cc + "expected:\n" + rc)
        }
    }

    /**
     * Make tests that process the files in path.  name is an identifying
     * name for this set of tests.  All files whose names end in srcext are
     * processed.  Processing is done by the function compile which must
     * return either Some (s) where s is the output or None if processing
     * failed.  If srcext is .x and resext is .y, then the expected result
     * for foo.x is found in file foo.y.  If optinext is Some (z), then
     * foo.z is used for standard input, if it exists, otherwise the string
     * indefault is used.  A test fails if either the processing fails or
     * it succeeds with the wrong result.  argslist is used to specify the
     * sets of command-line arguments that you want to use.  Each test is
     * run with each set of arguments.  The default is an empty argument list.
     */
    def filetests (name : String, path : String, srcext : String, resext : String,
                   optinext : Option[String] = None, indefault : String = "",
                   argslist : List[Array[String]] = List (Array ())) {

        import java.io.FilenameFilter

        /**
         * Make a set of file tests to produce the result files in dir.  For
         * each such result file found, we create a test.  That test takes as
         * input the corresponding input file (with extension inext), if it
         * exists, otherwise the input is the string indefault.  name is an
         * identifying string used in messages.  args is the array of extra
         * command line args to use.
         */
        def infiletests (c : String, dir : File, inext : String,
                         args : Array[String]) {
            val resfilter =
                new FilenameFilter {
                    def accept (dir : File, name : String) : Boolean = {
                        name.startsWith (c) && name.endsWith (resext)
                    }
                }
            val cp = path + "/" + c
            for (r <- dir.list (resfilter)) {
                val rp = path + "/" + r
                val ip = rp.replace (resext, inext)
                val inf = new File (ip)
                val (console, msg) =
                    if (inf.exists)
                        (new FileConsole (ip), " from input " + ip)
                    else
                        (new StringConsole (indefault), " from string \"" + indefault + "\"")
                filetest (name, cp, rp, console, msg, args)
            }
        }

        val dir = new File (path)
        val children = dir.list
        if (children == null) {
            throw (new IllegalArgumentException ("bad test file path " + path))
        } else {
            for (args <- argslist) {
                for (c <- children) {
                    if (c.endsWith (srcext)) {
                        optinext match {
                            case Some (inext) =>
                                infiletests (c, dir, inext, args)
                            case None =>
                                val cp = path + "/" + c
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
     * Make an AST from the file with the given name by parsing it with
     * the parser.
     */
    def makeast (reader : FileReader, filename : String) : Either[T,String] = {
        parseAll (parser, reader) match {
            case Success (ast, _) =>
                Left (ast)
            case f =>
                Right (f.toString)
        }
    }

}
