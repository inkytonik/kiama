/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * Trait to provide basic functionality for a compiler-like program
 * constructed from phases, including profiling and timing support.
 * `T` is the type of the syntax tree communicated from the parser
 * to the main processing of the compiler. `C` is the type of the
 * configuration.
 */
trait CompilerBase[T, C <: Config] extends PositionStore with Messaging with Profiler {

    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.rogach.scallop.exceptions.ScallopException

    /**
     * The entry point for this compiler.
     */
    def main(args : Array[String]) {
        driver(args)
    }

    /**
     * Create the configuration for a particular run of the compiler. Override
     * this if you have a custom configuration for your compiler.
     */
    def createConfig(args : Seq[String]) : C

    /**
     * Create and initialise the configuration for a particular run of the compiler.
     * Default: call `createConfig` and then initialise the resulting configuration.
     * Returns either the created configuration or an error message describing
     * why the configuration couldn't be created.
     */
    def createAndInitConfig(args : Seq[String]) : Either[String, C] = {
        try {
            val config = createConfig(args)
            config.verify()
            Right(config)
        } catch {
            case e : ScallopException =>
                Left(e.getMessage())
        }
    }

    /**
     * Driver for this compiler. First, use the argument list to create a
     * configuration for this execution. Then, use the configuration to
     * run the file processing in the appropriate way.
     */
    def driver(args : Seq[String]) {
        createAndInitConfig(args) match {
            case Left(message) =>
                println(message)
            case Right(config) =>
                if (config.profile.isDefined) {
                    val dimensions = parseProfileOption(config.profile())
                    profile(processfiles(config), dimensions, config.logging())
                } else if (config.time())
                    time(processfiles(config))
                else
                    processfiles(config)
        }
    }

    /**
     * Process the files one by one.
     */
    def processfiles(config : C) {
        for (filename <- config.filenames()) {
            processfile(filename, config)
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
    def processfile(filename : String, config : C) {
        try {
            val source = FileSource(filename, encoding)
            makeast(source, config) match {
                case Left(ast) =>
                    process(source, ast, config)
                case Right(messages) =>
                    report(messages, config.output())
            }
        } catch {
            case e : java.io.FileNotFoundException =>
                config.output().emitln(e.getMessage)
        }
    }

    /**
     * Make the contents of the given source, returning the AST wrapped in `Left`.
     * Return `Right` with messages if an AST cannot be made. `config` provides
     * access to all aspects of the configuration.
     */
    def makeast(source : Source, config : C) : Either[T, Messages]

    /**
     * Function to process the input that was parsed. `source` is the input
     * text processed by the compiler. `ast` is the abstract syntax tree
     * produced by the parser from that text. `config` provides access to all
     * aspects of the configuration.
     */
    def process(source : Source, ast : T, config : C)

    /**
     * Format an abstract syntax tree for printing. Default: return an empty document.
     */
    def format(ast : T) : Document

}

/**
 * A compiler that uses Parsers to produce positioned ASTs. `C` is the type of the
 * compiler configuration.
 */
trait CompilerWithConfig[T, C <: Config] extends CompilerBase[T, C] {

    import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, ParsersBase, Success}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{message, Messages}

    /**
     * The suite of parsers that is used by this compiler.
     */
    val parsers : ParsersBase

    /**
     * The particular parser used to parse this compiler's input.
     */
    val parser : parsers.Parser[T]

    /**
     * Make an AST by running the parser on the given source, returning messages
     * instead if the parse fails.
     */
    def makeast(source : Source, config : C) : Either[T, Messages] = {
        try {
            parsers.parseAll(parser, source) match {
                case Success(ast, _) =>
                    Left(ast)
                case res : NoSuccess =>
                    val pos = res.next.position
                    positions.setStart(res, pos)
                    positions.setFinish(res, pos)
                    val messages = message(res, res.message)
                    Right(messages)
            }
        } catch {
            case e : java.io.FileNotFoundException =>
                Right(message(e, e.getMessage))
        }
    }

}

/**
 * Specialisation of `CompilerWithConfig` that uses the default configuration
 * type.
 */
trait Compiler[T] extends CompilerWithConfig[T, Config] {

    def createConfig(args : Seq[String]) : Config =
        new Config(args)

}
