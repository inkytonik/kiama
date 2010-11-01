/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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

import org.kiama.util.ParsingREPL

/**
 * A simple typed lambda calculus read-eval-print-loop that offers
 * choice from among multiple evaluation mechanisms.  The lambda calculus
 * supported and the strategies used are heavily based on "Building
 * Interpreters with Rewriting Strategies", Eelco Dolstra and Eelco
 * Visser, LDTA 2002 (published in Volume 65/3 of Electronic Notes in
 * Theoretical Computer Science, Elsevier).
 */
object Lambda extends ParsingREPL[AST.Exp] with Parser {

    import Analysis._
    import Evaluators._
    import org.kiama.util.Messaging._

    override def setup (args : Array[String]) : Boolean = {
        println ("Enter lambda calculus expressions for evaluation (:help for help)")
        true
    }

    override def prompt () = "lambda2> "

    /**
     * Are we currently type-checking or not?  Default: true.
     */
    var typecheck = true

    /**
     * Print help about the available commands.
     */
    def help {
        println ("""exp                  print the result of evaluating exp
            |:eval                list the available evaluation mechanisms
            |:eval <mechanism>    change to using <mechanism> to evaluate
            |:type                print current type setting (default: on)
            |:type (on|off)       turn typing on or off""".stripMargin)
    }

    /**
     * Process a user input line by intercepting meta-level commands to
     * update the evaluation mechanisms.  By default we just parse what
     * they type into an expression.
     */
    override def processline (line : String) {
        line match {
            case Command (Array (":help")) =>
                help

            case Command (Array (":eval")) =>
                println ("Available evaluation mechanisms:")
                for (mech <- mechanisms) {
                    print ("  " + mech)
                    if (mech == mechanism)
                        println (" (current)")
                    else
                        println
                }

            case Command (Array (":eval", mech)) =>
                if (!setEvaluator (mech))
                    println ("unknown evaluation mechanism: " + mech)

            case Command (Array (":type")) =>
                println ("Typing is " + (if (typecheck) "on" else "off"))

            case Command (Array (":type", "on")) =>
                typecheck = true
                println ("Typing is on")

            case Command (Array (":type", "off")) =>
                typecheck = false
                println ("Typing is off")

            // Otherwise it's an expression for evaluation
            case _ => super.processline (line)
        }
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
     * Process an expression.
     */
    def process (e : AST.Exp) {
        if (typecheck) {
            // First conduct a semantic analysis check: compute the expression's
            // type and see if any errors occurred
            e->tipe
            if (messagecount == 0) {
                // If everything is OK, evaluate the expression
                println (evaluator.eval (e))
            } else {
                // Otherwise report the errors and reset for next expression
                report
                resetmessages
            }
        } else {
            println (evaluator.eval (e))
        }
    }

}
