/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2017 Anthony M Sloane, Macquarie University.
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

package org.bitbucket.inkytonik.kiama
package example.lambda2

import LambdaTree.Exp
import org.bitbucket.inkytonik.kiama.util.{
    ParsingREPLWithConfig,
    REPLConfig
}

/**
 * Configuration for the Lambda REPL.
 */
class LambdaConfig(args : Seq[String]) extends REPLConfig(args) {

    /**
     * Current evaluation mechanism (default: reduce).
     */
    var mechanism : String = "reduce"
}

/**
 * A simple typed lambda calculus read-eval-print-loop that offers
 * choice from among multiple evaluation mechanisms.  The lambda calculus
 * supported and the strategies used are heavily based on "Building
 * Interpreters with Rewriting Strategies", Eelco Dolstra and Eelco
 * Visser, LDTA 2002 (published in Volume 65/3 of Electronic Notes in
 * Theoretical Computer Science, Elsevier).
 */
class LambdaDriver extends ParsingREPLWithConfig[Exp, LambdaConfig] {

    import Evaluators.{evaluatorFor, mechanisms}
    import LambdaTree.LambdaTree
    import PrettyPrinter.formattedLayout
    import org.bitbucket.inkytonik.kiama.util.{Console, Source}

    def createConfig(args : Seq[String]) : LambdaConfig =
        new LambdaConfig(args)

    val banner = "Enter lambda calculus expressions for evaluation (:help for help)"

    val parsers = new SyntaxAnalyser(positions)
    val parser = parsers.exp

    /**
     * Process a user input line by intercepting meta-level commands to
     * update the evaluation mechanisms.  By default we just parse what
     * they type into an expression.
     */
    override def processline(source : Source, console : Console, config : LambdaConfig) : Option[LambdaConfig] = {

        // Shorthand access to the output emitter
        val output = config.output()

        /*
         * Print help about the available commands.
         */
        def printHelp() {
            output.emitln("""exp                  print the result of evaluating exp
                |:eval                list the available evaluation mechanisms
                |:eval <mechanism>    change to using <mechanism> to evaluate
                |:help                print this help message
                |:quit                quit this REPL""".stripMargin)
        }

        source.content match {
            case Command(Array(":help")) =>
                printHelp()
                Some(config)

            case Command(Array(":quit")) =>
                None

            case Command(Array(":eval")) =>
                output.emitln("Available evaluation mechanisms:")
                for (mech <- mechanisms) {
                    output.emit(s"  $mech")
                    if (mech == config.mechanism)
                        output.emitln(" (current)")
                    else
                        output.emitln
                }
                Some(config)

            case Command(Array(":eval", mech)) =>
                if (mechanisms contains mech) {
                    config.mechanism = mech
                    Some(config)
                } else {
                    output.emitln(s"unknown evaluation mechanism: $mech")
                    Some(config)
                }

            // Otherwise it's an expression for evaluation
            case _ =>
                super.processline(source, console, config)
        }

    }

    /**
     * Extractor for commands, splits the line into separate words.
     */
    object Command {
        def unapply(line : String) : Option[Array[String]] = {
            Some(line split ' ')
        }
    }

    /**
     * Process an expression.
     */
    def process(source : Source, e : Exp, config : LambdaConfig) {
        // Make an analyser for a tree for this expression
        val tree = new LambdaTree(e)
        val analyser = new Analyser(tree)

        // First conduct a semantic analysis check: compute the expression's
        // type and see if any errors occurred
        val messages = analyser.errors
        if (messages.length == 0) {
            // If everything is OK, evaluate the expression
            val evaluator = evaluatorFor(config.mechanism)
            config.output().emitln(formattedLayout(evaluator.eval(e)))
        } else {
            // Otherwise report the errors
            report(messages, config.output())
        }
    }

}

/**
 * Main object for Lambda REPL.
 */
object Lambda extends LambdaDriver
