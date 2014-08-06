/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package example.lambda3

import org.kiama.util.{ParsingREPL, PositionedParserUtilities}

/**
 * A simple lambda calculus using abstracted name binding.
 * The basic term syntax is augmented with query commands for the REPL.
 */
object LambdaTree {

    import org.kiama.rewriting.NominalTree.{Bind, Name, Trans}

    /**
     * Base type for all lambda tree nodes.
     */
    sealed abstract class LambdaNode

    /**
     * Lambda calculus expressions.
     */
    sealed abstract class Exp extends LambdaNode

    /**
     * Numeric expression.
     */
    case class Num (i : Int) extends Exp {
        override def toString : String = i.toString
    }

    /**
     * Variable expression.
     */
    case class Var (x : Name) extends Exp {
        override def toString : String = x.toString
    }

    /**
     * Application of l to r.
     */
    case class App (e1 : Exp, e2 : Exp) extends Exp {
        override def toString : String = s"($e1 $e2)"
    }

    /**
     * Lambda expression containing an abstracted binding.
     */
    case class Lam (b : Bind) extends Exp {
        override def toString : String = s"(\\${b.name} . ${b.term})"
    }

    /**
     * A query that can be entered from the REPL and returns a value of
     * type T when executed. These values are not in the term language but
     * are used to represent user commands.
     */
    sealed abstract class Query[T] extends LambdaNode

    /**
     * A query that determines the alpha equivalence of two expressions.
     */
    case class EquivQuery (e1 : Exp, e2 : Exp) extends Query[Boolean]

    /**
     * A query that computes the value of an expression.
     */
    case class EvalQuery (e : Exp) extends Query[Exp]

    /**
     * A query that determines the free names in an expression.
     */
    case class FreeNamesQuery (e : Exp) extends Query[Set[Name]]

    /**
     * A query that determines whether a name is not free in an expression.
     */
    case class FreshQuery (n : Name, e : Exp) extends Query[Boolean]

    /**
     * A query that substitutes an expression `e1` for name `n` in another
     * expression `e2`.
     */
    case class SubstQuery (n : Name, e1 : Exp, e2 : Exp) extends Query[Exp]

    /**
     * A query that swaps two names in an expression.
     */
    case class SwapQuery (tr : Trans, e : Exp) extends Query[Exp]

}

/**
 * Parser for simple lambda calculus plus REPL queries.
 */
trait SyntaxAnalyser extends PositionedParserUtilities {

    import LambdaTree._
    import org.kiama.rewriting.NominalTree.{Bind, Name, Trans}

    lazy val parser =
        phrase (query)

    lazy val query : PackratParser[Query[_]] =
        exp ~ ("===" ~> exp) ^^ EquivQuery |
        ("fv" ~> exp) ^^ FreeNamesQuery |
        name ~ ("#" ~> exp) ^^ FreshQuery |
        ("[" ~> name) ~ ("-> " ~> exp <~ "]") ~ exp ^^ SubstQuery |
        trans ~ exp ^^ SwapQuery |
        exp ^^ EvalQuery

    lazy val trans : PackratParser[Trans] =
        "(" ~> name ~ ("<->" ~> name) <~ ")"

    lazy val exp : PackratParser[Exp] =
        exp ~ factor ^^ App |
        ("\\" ~> name) ~ ("." ~> exp) ^^ {
            case n ~ e => Lam (Bind (n, e))
        } |
        factor |
        failure ("expression expected")

    lazy val factor : PackratParser[Exp] =
        integer | variable | "(" ~> exp <~ ")"

    lazy val integer =
        "[0-9]+".r ^^ (s => Num (s.toInt))

    lazy val variable =
        name ^^ Var

    lazy val name =
        "[a-zA-Z]+[0-9]+".r ^^ (
            fullname => {
                val (base, index) = fullname.span (_.isLetter)
                Name (base, Some (index.toInt))
            }
        ) |
        "[a-zA-Z]+".r ^^ (
            base =>
                Name (base, None)
        )

}

/**
 * Evaluation methods for simple lambda calculus.
 */
class Evaluator {

    import LambdaTree._
    import org.kiama.rewriting.NominalTree.Bind
    import org.kiama.rewriting.NominalRewriter

    /**
     * The rewriter to use to perform the evaluation.
     */
    val rewriter = new NominalRewriter

    /**
     * Call-by-name evaluation.
     */
    def cbn_eval (e : Exp) : Exp =
        e match {
            case App (t1, t2) =>
                val w = cbn_eval (t1)
                w match {
                    case Lam (Bind (a, u : Exp)) =>
                        val v = rewriter.subst (a, t2) (u)
                        cbn_eval (v)
                    case _ =>
                        App (w, t2)
                }
            case _ =>
                e
        }

    /**
     * Query execution
     */
    def execute[T] (q : Query[T]) : T =
        q match {
            case EquivQuery (e1, e2)    => rewriter.alphaequiv (e1, e2)
            case EvalQuery (e)          => cbn_eval (e)
            case FreeNamesQuery (e)     => rewriter.fv (e)
            case FreshQuery (n, e)      => rewriter.fresh (n) (e)
            case SubstQuery (n, e1, e2) => rewriter.subst (n, e1) (e2)
            case SwapQuery (tr, e)      => rewriter.swap (tr) (e)
        }

}

/**
 * Simple lambda calculus implementation to illustrate Kiama's support for
 * nominal rewriting. This implementation is closely based on the example
 * used in Scrap your Nameplate, James Cheney, ICFP 2005.
 */
object Lambda extends ParsingREPL[LambdaTree.Query[_]] with SyntaxAnalyser {

    import org.kiama.util.REPLConfig

    val banner =
        """
        |Enter lambda calculus queries:
        |
        | e               evaluate e (e.g., (\x . x) 3)
        | (n1 <-> n2) e   swap n1 and n2 in e
        | n # e           is n fresh in e?
        | fv e            free variables of e
        | e1 === e2       is e1 alpha equivalent to e2?
        | [n -> e1] e2    substitute e1 for n in e2
        |
        |where n = name, e = expression
        |
        """.stripMargin

    override val prompt = "query> "

    val evaluator = new Evaluator

    override def process (q : LambdaTree.Query[_], config : REPLConfig) {
        super.process (q, config)
        config.output.emitln (evaluator.execute (q))
    }

}
