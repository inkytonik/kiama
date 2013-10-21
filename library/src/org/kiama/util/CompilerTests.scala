/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
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

import org.scalatest.FunSuiteLike

/**
 * Basic tests of compiler module.  Normal usage is tested by many of
 * the examples.
 */
class CompilerTests extends Tests with CompilerBase[Any,Config] with TestCompiler[Any] {

    import java.io.Reader
    import org.scalatest.TestFailedException

    def createConfig (args : Array[String], emitter : Emitter = new Emitter) : Config =
        new Config (args, emitter)

    def makeast (reader : Reader, filename : String, config : Config) : Either[Any,String] =
         Right ("Dummy")

    test ("compiler driver produces an appropriate message if a file is not found") {
        val emitter = new StringEmitter
        val config = createConfig (Array ("IDoNotExist.txt"), emitter)
        testdriver (config)
        val expectedMsg =
            if (System.getProperty("os.name").startsWith ("Windows"))
                "The system cannot find the file specified"
            else
                "No such file or directory"
        assertResult (s"IDoNotExist.txt ($expectedMsg)\n") (emitter.result)
    }

    test ("filetests using a directory that doesn't exist fails") {
        val i = intercept[IllegalArgumentException] {
                    filetests ("Compiler", "library/src/org/kiama/util/IDoNotExist", ".src", ".out")
                }
        assertResult ("bad test file path library/src/org/kiama/util/IDoNotExist") (i.getMessage)
    }

}

/**
 * Support for testing compiler drivers.
 */
trait TestCompilerWithConfig[T, C <: Config] extends FunSuiteLike {

    self : CompilerBase[T,C] =>

    import java.io.File
    import org.kiama.attribution.Attribution
    import org.kiama.util.Config
    import scala.io.Source

    /**
     * Run the compiler in test mode using the given configuration.
     */
    def testdriver (config : C) {
        Attribution.resetMemo
        Messaging.resetmessages
        processfiles (config.filenames (), config)
    }

    /**
     * Flag to decide whether to sanitise the output before comparison
     * of test results with expected results (see `sanitise` method).
     * Default is true; override with false if you want actual results
     * compared.
     */
    def dosanitisation : Boolean =
        true

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
     * Make tests that process the files in path.  `name` is an identifying
     * name for this set of tests.  All files whose names end in `srcext` are
     * processed.  Processing is done by the function `compile` which must
     * return either `Some (s)` where `s` is the output or `None` if processing
     * failed.  If `srcext` is `.x` and `resext` is `.y`, then the expected result
     * for `foo.x` is found in file `foo.y`.  If `optinext` is `Some (z)`, then
     * `foo.z` is used for standard input, if it exists, otherwise the string
     * `indefault` is used.  A test fails if either the processing fails or
     * it succeeds with the wrong result.  `argslist` is used to specify the
     * sets of command-line arguments that you want to use.  Each test is
     * run with each set of arguments.  The default is an empty argument list.
     */
    def filetests (name : String, path : String, srcext : String, resext : String,
                   optinext : Option[String] = None, indefault : String = "",
                   argslist : List[Array[String]] = List (Array ())) {

        import java.io.FilenameFilter

        /**
         * Make a single file test processing using the command-line `cmd`,
         * expecting output as in the file `rp`.  The `extra` string is appended
         * to the normal test title. `name` is an identifying string used in
         * messages. If the compilation fails, `rp` is assumed to contain the
         * expected messages. `rt` is a version of `rp` to use in the test title.
         */
        def filetest (name : String, rp : String, cmd : Array[String], rt : String,
                      extra : String = "") {
            val ct = cmd.mkString (" ").replaceAllLiterally ("kiama/src/org/kiama/", "")
            val title = s"$name: $ct, expecting $rt$extra"
            test (title) {
                val emitter = new StringEmitter
                val config = createConfig (cmd, emitter)
                try {
                    testdriver (config)
                } catch {
                    case e : Exception =>
                        info ("failed with an exception ")
                        throw (e)
                }
                val cc = emitter.result
                try {
                    val rc = Source.fromFile (rp).mkString
                    assert (sanitise (cc) === sanitise (rc), s"$title generated bad output")
                } catch {
                    case e : java.io.FileNotFoundException =>
                        fail (s"$rp not found")
                }
            }
        }

        /**
         * Make a set of file tests to produce the result files in `dir`.  For
         * each such result file found, we create a test.  That test takes as
         * input the corresponding input file (with extension `inext`), if it
         * exists, otherwise the input is the string `indefault`.  `name` is an
         * identifying string used in messages.  `args` is the array of extra
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
            val cp = s"$path/$c"
            for (r <- dir.list (resfilter)) {
                val rp = s"$path/$r"
                val it = r.replace (resext, inext)
                val ip = s"$path/$it"
                val inf = new File (ip)
                val (consoleArgs, msg) =
                    if (inf.exists)
                        (Array ("-c", "file", ip), s" from input $it")
                    else
                        (Array ("-c", "string", indefault), s""" from string "$indefault"""")
                filetest (name, rp, consoleArgs ++ args :+ cp, r, msg)
            }
        }

        val dir = new File (path)
        val children = dir.list
        if (children == null) {
            throw (new IllegalArgumentException (s"bad test file path $path"))
        } else {
            for (args <- argslist) {
                for (c <- children) {
                    if (c.endsWith (srcext)) {
                        optinext match {
                            case Some (inext) =>
                                infiletests (c, dir, inext, args)
                            case None =>
                                val cp = s"$path/$c"
                                val rt = c.replace (srcext, resext)
                                val rp = s"$path/$rt"
                                filetest (name, rp, args :+ cp, rt)
                        }
                    }
                }
            }
        }

    }

}

/**
 * Specialisation of `TestCompilerWithConfig` that uses the default
 * configuration type.
 */
trait TestCompiler[T] extends TestCompilerWithConfig[T,Config] {

    self : CompilerBase[T,Config] =>

}
