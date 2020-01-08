/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * Basic tests of compiler module.  Normal usage is tested by many of
 * the examples.
 */
class CompilerTests extends KiamaTests with Compiler[Any, Any] with TestCompiler[Any, Any] {

    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{emptyDocument, Document}
    import org.bitbucket.inkytonik.kiama.parsing.{Parsers, ParseResult}
    import org.bitbucket.inkytonik.kiama.util.Source

    val name = "compiler"

    class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {
        val dummy : Parser[String] = "dummy".r
    }

    def parse(source : Source) : ParseResult[String] = {
        val parsers = new SyntaxAnalyser(positions)
        parsers.parseAll(parsers.dummy, source)
    }

    def process(source : Source, ast : Any, config : Config) : Unit = {
        // Do nothing
    }

    def format(m : Any) : Document =
        emptyDocument

    test("compiler driver produces an appropriate message if a file is not found") {
        createAndInitConfig(Vector("--Koutput", "string", "IDoNotExist.txt")) match {
            case Left(message) =>
                fail(message)
            case Right(config) =>
                val expectedMsg =
                    if (System.getProperty("os.name").startsWith("Windows"))
                        "The system cannot find the file specified"
                    else
                        "No such file or directory"
                testdriver(config)
                config.stringEmitter.result shouldBe s"IDoNotExist.txt ($expectedMsg)\n"
        }
    }

    test("compiler driver produces an appropriate message if a command-line option is unknown") {
        createAndInitConfig(Vector("--broken", "string", "IDoNotExist.txt")) match {
            case Left(message) =>
                message shouldBe "Unknown option 'broken'"
            case Right(config) =>
            // do nothing
        }
    }

    test("filetests using a directory that doesn't exist fails") {
        val i = intercept[IllegalArgumentException] {
            filetests("Compiler", "util/IDoNotExist", ".src", ".out")
        }
        i.getMessage shouldBe "bad test file path src/test/scala/org/bitbucket/inkytonik/kiama/util/IDoNotExist"
    }

}

/**
 * Support for testing drivers.
 */
trait TestDriverWithConfig[C <: Config] extends KiamaTests {

    import java.io.File
    import scala.io.Source

    /**
     * Create the configuration for a particular run of the program. Override
     * this if you have a custom configuration for your compiler.
     */
    def createConfig(args : Seq[String]) : C

    /**
     * Create and initialise the configuration for a particular run of the REPL.
     * Default: call `createConfig` and then initialise the resulting configuration.
     * Returns either the created configuration or an error message describing
     * why the configuration couldn't be created.
     */
    def createAndInitConfig(args : Seq[String]) : Either[String, C]

    /**
     * Run the driver in test mode using the given configuration.
     */
    def testdriver(config : C) : Unit

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
    def sanitise(s : String) : String =
        if (dosanitisation)
            s.replaceAll("\r", "\n")
        else
            s

    /*
        * Make a single file test processing using the command-line `args`,
        * expecting output as in the file `rp`.  The `extra` string is appended
        * to the normal test title. `name` is an identifying string used in
        * messages. If the compilation fails, `rp` is assumed to contain the
        * expected messages. `rt` is a version of `rp` to use in the test title.
        */
    def filetest(name : String, path : String, rp : String, args : Seq[String], rt : String,
        extra : String = "") : Unit = {
        val ct = args.mkString(" ").replaceAllLiterally(path + "/", "")
        val title = s"$name: $ct, expecting $rt$extra"
        test(title) {
            createAndInitConfig("--Koutput" +: "string" +: args) match {
                case Left(message) =>
                    fail(message)
                case Right(config) =>
                    try {
                        testdriver(config)
                    } catch {
                        case e : Exception =>
                            info("failed with an exception ")
                            throw (e)
                    }
                    val cc = config.stringEmitter.result
                    val rc = Source.fromFile(rp).mkString
                    sanitise(cc) shouldBe sanitise(rc)
            }
        }
    }

    /**
     * Make tests that process the files in relPath which is relative to the
     * top of the Kiama library test sources. `name` is an identifying
     * name for this set of tests.  All files whose names end in `srcext` are
     * processed.  Processing is done by the function `testdriver` which
     * is given a configuration which has an emitter. All output of the run
     * is captured by the emitter. If `srcext` is `.x` and `resext` is `.y`,
     * then the expected result for `foo.x` is found in file `foo.y`.  If
     * `optinext` is `Some (z)`, then `foo.z` is used for standard input, if
     * it exists, otherwise the string `indefault` is used.  A test fails if
     * either the processing fails or it succeeds with the wrong result.
     * `argslist` is used to specify the sets of command-line arguments that
     * you want to use.  Each test is run with each set of arguments.  The
     * default is an empty argument list.
     */
    def filetests(name : String, relPath : String, srcext : String, resext : String,
        optinext : Option[String] = None, indefault : String = "",
        argslist : List[Seq[String]] = List(Seq())) : Unit = {

        import java.io.FilenameFilter

        val basePath = "src/test/scala/org/bitbucket/inkytonik/kiama/"
        val base = new File(basePath)
        val path = if (base.exists) basePath + relPath else relPath

        /*
         * Make a set of file tests to produce the result files in `dir`.  For
         * each such result file found, we create a test.  That test takes as
         * input the corresponding input file (with extension `inext`), if it
         * exists, otherwise the input is the string `indefault`.  `name` is an
         * identifying string used in messages.  `args` is the array of extra
         * command line args to use.
         */
        def infiletests(c : String, dir : File, inext : String,
            args : Seq[String]) : Unit = {
            val resfilter =
                new FilenameFilter {
                    def accept(dir : File, name : String) : Boolean = {
                        name.startsWith(c) && name.endsWith(resext)
                    }
                }
            val cp = s"$path/$c"
            for (r <- dir.list(resfilter)) {
                val rp = s"$path/$r"
                val it = r.replace(resext, inext)
                val ip = s"$path/$it"
                val inf = new File(ip)
                val (consoleArgs, msg) =
                    if (inf.exists)
                        (Vector("--Kconsole", "file", ip), s" from input $it")
                    else
                        (Vector("--Kconsole", "string", indefault), s" from string '$indefault'")
                filetest(name, path, rp, consoleArgs ++ args :+ "--" :+ cp, r, msg)
            }
        }

        val dir = new File(path)
        val children = dir.list
        if (children == null) {
            throw (new IllegalArgumentException(s"bad test file path $path"))
        } else {
            for (args <- argslist) {
                for (c <- children) {
                    if (c.endsWith(srcext)) {
                        optinext match {
                            case Some(inext) =>
                                infiletests(c, dir, inext, args)
                            case None =>
                                val cp = s"$path/$c"
                                val rt = c.replace(srcext, resext)
                                val rp = s"$path/$rt"
                                filetest(name, path, rp, args :+ "--" :+ cp, rt)
                        }
                    }
                }
            }
        }

    }

}

/**
 * Support for testing compiler drivers.
 */
trait TestCompilerWithConfig[N, T <: N, C <: Config] extends TestDriverWithConfig[C] {

    self : CompilerBase[N, T, C] =>

    /**
     * Run the compiler in test mode using the given configuration
     * with reset position store.
     */
    def testdriver(config : C) : Unit = {
        positions.reset()
        compileFiles(config)
    }

}

/**
 * Specialisation of `TestCompilerWithConfig` that uses the default
 * configuration type.
 */
trait TestCompiler[N, T <: N] extends TestCompilerWithConfig[N, T, Config] {

    self : CompilerBase[N, T, Config] =>

}
