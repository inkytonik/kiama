/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * A REPL that uses ScalaCheck to generate random instances of abstract
 * syntax trees of type T and prints them using a configurable emitter.
 */
trait GeneratingREPLBase[T] extends REPL {

    import org.scalacheck._
    import org.scalacheck.rng.Seed

    val banner = "Each time you hit ENTER a new instance is generated and printed."

    override val prompt = "Hit ENTER to generate an instance: "

    /**
     * Generating REPLs insist on processing whitespace.
     */
    override def createConfig(args : Seq[String]) : REPLConfig =
        super.createConfig("--KprocessWhitespaceLines" +: args)

    /**
     * The generator to use to make values of type T.
     */
    def generator : Arbitrary[T]

    /**
     * Generate a new instance and print it, ignoring the input line. Return
     * the configuration unchanged.
     */
    def processline(source : Source, console : Console, config : REPLConfig) : Option[REPLConfig] = {
        generator.arbitrary(Gen.Parameters.default, Seed.random()) match {
            case Some(t) =>
                process(source, t, config)
            case None =>
                config.output().emitln("can't generate an instance")
        }
        Some(config)
    }

    /**
     * Process a generated value.  Default: print it.
     */
    def process(source : Source, t : T, config : REPLConfig) {
        config.output().emitln(t)
    }

}

/**
 * A REPL that uses ScalaCheck to generate random instances of abstract
 * syntax trees of type T and prints them to standard output.
 */
trait GeneratingREPL[T] extends GeneratingREPLBase[T]

/**
 * Support for testing REPL drivers.
 */
trait TestREPLWithConfig[C <: REPLConfig] extends TestDriverWithConfig[C] {

    self : REPLBase[C] =>

    /**
     * Run the REPL in test mode using the given configuration.
     */
    def testdriver(config : C) {
        processfiles(config)
    }

}

/**
 * Specialisation of `TestREPLWithConfig` that uses the default
 * configuration type.
 */
trait TestREPL extends TestREPLWithConfig[REPLConfig] {

    self : REPLBase[REPLConfig] =>

}
