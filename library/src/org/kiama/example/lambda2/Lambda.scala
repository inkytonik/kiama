/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2013 Anthony M Sloane, Macquarie University.
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
package example.lambda2

import org.kiama.util.{Emitter, ParsingREPLWithConfig, REPLConfig}

/**
 * Configuration for the Lambda REPL.
 */
class LambdaConfig (args : Array[String], emitter : Emitter) extends REPLConfig (args, emitter) {
    val mechanism = opt[String] ("mechanism", descr = "Evaluation mechanism",
                                 default = Some ("reduce"))
}

/**
 * A simple typed lambda calculus read-eval-print-loop that offers
 * choice from among multiple evaluation mechanisms.  The lambda calculus
 * supported and the strategies used are heavily based on "Building
 * Interpreters with Rewriting Strategies", Eelco Dolstra and Eelco
 * Visser, LDTA 2002 (published in Volume 65/3 of Electronic Notes in
 * Theoretical Computer Science, Elsevier).
 */
object Lambda extends ParsingREPLWithConfig[AST.Exp,LambdaConfig] with Parser {

    import Evaluators.{evaluatorFor, mechanisms}
    import PrettyPrinter._
    import org.kiama.util.Emitter
    import org.kiama.util.Messaging

    def createConfig (args : Array[String], emitter : Emitter = new Emitter) : LambdaConfig =
        new LambdaConfig (args, emitter)

    val banner = "Enter lambda calculus expressions for evaluation (:help for help)"

    /**
     * Process a user input line by intercepting meta-level commands to
     * update the evaluation mechanisms.  By default we just parse what
     * they type into an expression.
     */
    override def processline (line : String, config : LambdaConfig) : LambdaConfig = {

        // Shorthand access to the emitter
        val emitter = config.emitter

        /**
         * Print help about the available commands.
         */
        def help {
            config.emitter.emitln ("""exp                  print the result of evaluating exp
                |:eval                list the available evaluation mechanisms
                |:eval <mechanism>    change to using <mechanism> to evaluate""".stripMargin)
        }

        line match {
            case Command (Array (":help")) =>
                help

            case Command (Array (":eval")) =>
                emitter.emitln ("Available evaluation mechanisms:")
                for (mech <- mechanisms) {
                    emitter.emit (s"  $mech")
                    if (mech == config.mechanism ())
                        emitter.emitln (" (current)")
                    else
                        emitter.emitln
                }

            case Command (Array (":eval", mech)) =>
                if (mechanisms contains mech)
                    return createConfig (Array ("-m", mech), emitter)
                else
                    emitter.emitln (s"unknown evaluation mechanism: $mech")

            // Otherwise it's an expression for evaluation
            case _ =>
                super.processline (line, config)
        }

        config
    }

    /**
     * Extractor for commands, splits the line into separate words.
     */
    object Command {
        def unapply (line : String) : Option[Array[String]] = {
            Some (line split ' ')
        }
    }

    /**
     * The messaging module to use during processing.
     */
    val messaging = new Messaging

    /**
     * The analysis object to use for processing.
     */
    val analysis = new Analysis (messaging)

    /**
     * Process an expression.
     */
    override def process (e : AST.Exp, config : LambdaConfig) {
        super.process (e, config)
        // First conduct a semantic analysis check: compute the expression's
        // type and see if any errors occurred
        analysis.tipe (e)
        if (messaging.messagecount == 0) {
            // If everything is OK, evaluate the expression
            val evaluator = evaluatorFor (config.mechanism ())
            config.emitter.emitln (pretty (evaluator.eval (e)))
        } else {
            // Otherwise report the errors and reset for next expression
            messaging.report (config.emitter)
        }
    }

}
