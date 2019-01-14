/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.lambda

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.{ParsingREPL, Positions}

/**
 * A simple lambda calculus.
 */
object LambdaTree {

    /**
     * Identifiers are represented as strings.
     */
    type Idn = String

    /**
     * Base type for all lambda tree nodes.
     */
    sealed abstract class LambdaNode

    /**
     * Expressions.
     */
    sealed abstract class Exp extends LambdaNode

    /**
     * Numeric expressions.
     */
    case class Num(i : Int) extends Exp {
        override def toString : String = i.toString
    }

    /**
     * Variable expressions.
     */
    case class Var(x : Idn) extends Exp {
        override def toString : String = x
    }

    /**
     * Lambda expressions binding x within e.
     */
    case class Lam(x : Idn, e : Exp) extends Exp {
        override def toString : String = s"(\\$x.$e)"
    }

    /**
     * Application of l to r.
     */
    case class App(l : Exp, r : Exp) extends Exp {
        override def toString : String = s"($l $r)"
    }

    /**
     * Substitution of n for x within m.
     */
    case class Sub(m : Exp, x : Idn, n : Exp) extends Exp

}

/**
 * Parser to abstract syntax tree for simple lambda calculus.
 */
class SyntaxAnalyser(positions : Positions) extends Parsers(positions) {

    import LambdaTree._

    lazy val exp : PackratParser[Exp] =
        exp ~ factor ^^ App |
            ("\\" ~> idn) ~ ("." ~> exp) ^^ Lam |
            factor |
            failure("expression expected")

    lazy val factor =
        integer | variable | "(" ~> exp <~ ")"

    lazy val integer =
        "[0-9]+".r ^^ (s => Num(s.toInt))

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
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
    import org.bitbucket.inkytonik.kiama.util.Counter

    /**
     * Free variables
     */
    def fv(t : Exp) : Set[Idn] = {
        t match {
            case Num(_) =>
                Set()
            case Var(x) =>
                Set(x)
            case Lam(x, e) =>
                fv(e) - x
            case App(m, n) =>
                fv(m) ++ fv(n)
            case Sub(m, x, n) =>
                (fv(m) - x) ++ fv(n)
        }
    }

    /**
     * Fresh variable name generator. To keep things simple, we generate
     * names of the form `_vn` where `n` is a unique number. These names
     * are guaranteed not to occur in user programs, so we do not have
     * to check for clashes.
     */
    object FreshVar extends Counter {

        def apply() : Idn =
            s"_v${next()}"

    }

    /**
     * \xgc-reduction
     */
    val xgc_reduction =
        rule[Exp] {
            // Substitution generation
            case App(Lam(x, e1), e2) =>
                Sub(e1, x, e2)

            // Explicit substitution
            case Sub(Var(x), y, n) if x == y =>
                n
            case Sub(Var(x), _, _) =>
                Var(x)
            case Sub(Lam(x, m), y, n) =>
                val xprime = FreshVar()
                Lam(xprime, Sub(Sub(m, x, Var(xprime)), y, n))
            case Sub(App(m1, m2), y, n) =>
                App(Sub(m1, y, n), Sub(m2, y, n))

            // Garbage collection
            case Sub(m, x, n) if !(fv(m) contains (x)) =>
                m
        }

    /**
     * Normal-order reduction
     */
    val normal = outermost(xgc_reduction)

    /**
     * Entry poiint for running reduction-based evaluator.
     */
    def evaluate(e : Exp) : Exp =
        rewrite(normal)(e)

}

/**
 * A read-eval-print loop for evaluation of lambda calculus expressions.
 */
class LambdaDriver extends ParsingREPL[LambdaTree.Exp] with Evaluator {

    import org.bitbucket.inkytonik.kiama.util.{REPLConfig, Source}

    val banner = "Enter lambda calculus expressions for evaluation."

    override val prompt = "lambda> "

    val parsers = new SyntaxAnalyser(positions)
    val parser = parsers.exp

    def process(source : Source, e : LambdaTree.Exp, config : REPLConfig) {
        val result =
            if (config.profile.isDefined) {
                val dimensions = profiler.parseProfileOption(config.profile())
                profiler.profile(normal(e), dimensions, config.logging())
            } else
                normal(e)
        config.output().emitln(result.getOrElse("reduction failed"))
    }

}

/**
 * Main object for Lambda REPL.
 */
object Lambda extends LambdaDriver
