/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2014 Anthony M Sloane, Macquarie University.
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

import java.io.Reader
import org.kiama.attribution.Attributable
import scala.collection.immutable.Seq
import scala.util.parsing.combinator.RegexParsers

/**
 * Trait to provide basic functionality for a compiler-like program
 * constructed from phases, including profiling and timing support.
 * `T` is the type of the syntax tree communicated from the parser
 * to the main processing of the compiler. `C` is the type of the
 * configuration.
 */
trait CompilerBase[T, C <: Config] extends Profiler {

    import org.kiama.util.{Console, Emitter, StringEmitter}
    import org.kiama.util.IO.{filereader, FileNotFoundException}
    import scala.io.Source

    /**
     * The entry point for this compiler.
     */
    def main (args : Array[String]) {
        driver (args.toIndexedSeq)
    }

    /**
     * Create the configuration for a particular run of the compiler.
     * If supplied, use `emitter` instead of a standard output emitter.
     */
    def createConfig (args : Seq[String],
                      output : Emitter = new OutputEmitter,
                      error : Emitter = new ErrorEmitter) : C

    /**
     * Driver for this compiler. First, use the argument list to create a
     * configuration for this execution. Then, use the configuration to
     * run the file processing in the appropriate way.
     */
    def driver (args : Seq[String]) {
        val config = createConfig (args)
        if (config.profile.get != None) {
            val dimensions = parseProfileOption (config.profile ())
            profile (processfiles (config.filenames (), config), dimensions,
                                   config.logging ())
        } else if (config.time ())
            time (processfiles (config.filenames (), config))
        else
            processfiles (config.filenames (), config)
    }

    /**
     * Process the files one by one.
     */
    def processfiles (filenames : Seq[String], config : C) {
        for (filename <- filenames) {
            processfile (filename, config)
        }
    }

    /**
     * The character encoding of input files read by this compiler.
     * Defaults to UTF-8.
     */
    def encoding : String =
        "UTF-8"

    /**
     * Process a file argument by using `makeast` to turn their contents into
     * abstract syntax trees (ASTs) and then by process which conducts arbitrary
     * processing on the ASTs. The character encoding of the files is given by
     * the `encoding` method.
     */
    def processfile (filename : String, config : C) {
        try {
            val reader = filereader (filename, encoding)
            makeast (reader, filename, config) match {
                case Left (ast) =>
                    process (filename, ast, config)
                case Right (msg) =>
                    config.error.emitln (msg)
            }
        } catch {
            case e : FileNotFoundException =>
                config.error.emitln (e.getMessage)
        }
    }

    /**
     * Make an AST from the file with the given name, returning it wrapped in
     * `Left`.  Returns `Right` with an error message if an AST cannot be made.
     * `config` provides access to all aspects of the configuration.
     */
    def makeast (reader : Reader, filename : String, config : C) : Either[T,String]

    /**
     * Function to process the input that was parsed. `filename` is the name
     * of the file from which the input came. `ast` is the abstract syntax tree
     * produced by the parser from that file. `config` provides access to all
     * aspects of the configuration. The default implmentation does nothing.
     */
    def process (filename : String, ast : T, config : C) {
        // Do nothing
    }

}

/**
 * A compiler that uses RegexParsers to produce Attributable ASTs. The AST
 * is initialised with `initTree` by `process`. Override it and call it
 * before performing specific attribution. `C` is the type of the compiler
 * configuration.
 */
trait CompilerWithConfig[T <: Attributable, C <: Config] extends CompilerBase[T,C] with RegexParsers {

    import org.kiama.attribution.Attribution.initTree

    /**
     * The actual parser used to produce the AST.
     */
    def parser : Parser[T]

    /**
     * Make an AST by running the parser, reporting errors if the parse fails.
     */
    def makeast (reader : Reader, filename : String, config : C) : Either[T,String] =
        parseAll (parser, reader) match {
            case Success (ast, _) =>
                Left (ast)
            case f =>
                Right (f.toString)
        }

    /**
     * Process the AST by performing any processing at the next level up
     * and then initialising the AST for attribution.
     */
    override def process (filename : String, ast : T, config : C) {
        super.process (filename, ast, config)
        initTree (ast)
    }

}

/**
 * Specialisation of `CompilerWithConfig` that uses the default configuration
 * type.
 */
trait Compiler[T <: Attributable] extends CompilerWithConfig[T,Config] {

    def createConfig (args : Seq[String],
                      output : Emitter = new OutputEmitter,
                      error : Emitter = new ErrorEmitter) : Config =
        new Config (args, output, error)

}
