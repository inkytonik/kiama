/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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
package example.lambda

import org.kiama.util.{ParsingREPL, PositionedParserUtilities, Profiler}

/**
 * A simple lambda calculus.
 */
object LambdaTree {

    import org.kiama.util.TreeNode

    /**
     * Identifiers are represented as strings.
     */
    type Idn = String

    /**
     * Expressions.
     */
    sealed abstract class Exp extends TreeNode

    /**
     * Numeric expressions.
     */
    case class Num (i : Int) extends Exp {
        override def toString : String = i.toString
    }

    /**
     * Variable expressions.
     */
    case class Var (x : Idn) extends Exp {
        override def toString : String = x
    }

    /**
     * Lambda expressions binding x within e.
     */
    case class Lam (x : Idn, e : Exp) extends Exp {
        override def toString : String = s"(\\$x.$e)"
    }

    /**
     * Application of l to r.
     */
    case class App (l : Exp, r : Exp) extends Exp {
        override def toString : String = s"($l $r)"
    }

    /**
     * Substitution of n for x within m.
     */
    case class Sub (m : Exp, x : Idn, n : Exp) extends Exp

}

/**
 * Parser to abstract syntax tree for simple lambda calculus.
 */
trait SyntaxAnalyser extends PositionedParserUtilities {

    import LambdaTree._

    lazy val parser =
        phrase (exp)

    lazy val exp : PackratParser[Exp] =
        exp ~ factor ^^ App |
        ("\\" ~> idn) ~ ("." ~> exp) ^^ Lam |
        factor |
        failure ("expression expected")

    lazy val factor : PackratParser[Exp] =
        integer | variable | "(" ~> exp <~ ")"

    lazy val integer =
        "[0-9]+".r ^^ (s => Num (s.toInt))

    lazy val variable =
        idn ^^ Var

    lazy val idn =
        "[a-zA-Z][a-zA-Z0-9]*".r

}

/**
 * Lambda calculus evaluator following Rose's \xgc, ie with explicit
 * substitutions and garbage collection.  See "Explicit Substitution
 * - Tutorial and Survey", Kristoffer H. Rose, BRICS LS-96-3, October
 * 1996.
 */
trait Evaluator {

    import LambdaTree._
    import org.kiama.rewriting.Rewriter._

    /**
     * Free variables
     */
    def fv (t : Exp) : Set[Idn] = {
        t match {
            case Num (_) =>
                Set ()
            case Var (x) =>
                Set (x)
            case Lam (x, e) =>
                fv (e) - x
            case App (m, n) =>
                fv (m) ++ fv (n)
            case Sub (m, x, n) =>
                (fv (m) - x) ++ fv (n)
        }
    }

    /**
     * \xgc-reduction
     */
    val xgc_reduction =
        rule[Exp] {
            // Substitution generation
            case App (Lam (x, e1), e2) =>
                Sub (e1, x, e2)

            // Explicit substitution
            case Sub (Var (x), y, n) if x == y =>
                n
            case Sub (Var (x), _, _) =>
                Var (x)
            case Sub (Lam (x, m), y, n) =>
                Lam (x, Sub (m, y, n))
            case Sub (App (m1, m2), y, n) =>
                App (Sub (m1, y, n), Sub (m2, y, n))

            // Garbage collection
            case Sub (m, x, n) if ! (fv (m) contains (x)) =>
                m
        }

    /**
     * Normal-order reduction
     */
    val normal = outermost (xgc_reduction)

}

/**
 * A read-eval-print loop for evaluation of lambda calculus expressions.
 */
object Lambda extends ParsingREPL[LambdaTree.Exp] with SyntaxAnalyser with Evaluator with Profiler {

    import org.kiama.util.REPLConfig

    val banner = "Enter lambda calculus expressions for evaluation."

    override val prompt = "lambda> "

    override def process (e : LambdaTree.Exp, config : REPLConfig) {
        super.process (e, config)
        val result =
            if (config.profile.get != None) {
                val dimensions = parseProfileOption (config.profile ())
                profile (normal (e), dimensions, config.logging ())
            } else
                normal (e)
        config.emitter.emitln (result.getOrElse ("reduction failed"))
    }

}
