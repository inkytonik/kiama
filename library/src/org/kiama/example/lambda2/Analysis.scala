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

import org.kiama.util.Messaging

/**
 * Analyses for typed lambda calculus expressions.  A simple free variable
 * analysis plus name and type analysis.  There are two versions of the
 * latter here: one (tipe) that constructs an explicit environment separate
 * from the AST, and one (tipe2) that represents names by references to the
 * nodes of their binding lambda expressions.
 */
class Analysis (val messaging : Messaging) {

    import LambdaTree._
    import PrettyPrinter._
    import messaging.message
    import org.kiama.attribution.Attribution._
    import scala.collection.immutable.Seq

    /**
     * The variables that are free in the given expression.
     */
    val fv : Exp => Set[Idn] =
        attr {
            case Num (_)            => Set ()
            case Var (v)            => Set (v)
            case Lam (v, _, e)      => fv (e) -- Set (v)
            case App (e1, e2)       => fv (e1) ++ fv (e2)
            case Opn (e1, _, e2)    => fv (e1) ++ fv (e2)
            case Let (i, t, e1, e2) => fv (e1) ++ fv (e2) -- Set (i)
            case Letp (bs, e)       => val fbv = bs.map (_.e).map (fv).flatten.toSet
                                       val bvars = bs.map (_.i).toSet
                                       fbv ++ (fv (e) -- bvars)
        }

    /**
     * The environment of an expression is the list of variable names that
     * are visible in that expression and their types.
     */
    val env : Exp => Seq[(Idn,Type)] =
        attr {

            // Nothing is visible at the root of the tree
            case p if p.isRoot =>
                Seq ()

            // Othrewise, look at the context
            case n =>
                (n.parent) match {

                    // Inside a lambda expression the bound variable is now visible
                    // in addition to everything that is visible from above. Note
                    // that an inner declaration of a var hides an outer declaration
                    // of the same var since we add inner bindings at the beginning
                    // of the env and we search the env list below in tipe from
                    // beginning to end
                    case p @ Lam (x, t, _) => (x,t) +: (p->env)

                    // Other expressions do not bind new identifiers so they just
                    // get their environment from their parent
                    case p : Exp           => p->env

                }

        }

    /**
     * Return whether the type of `e` is `expectedType` or unknown. If it
     * isn't either of these, also report an error.
     */
    def checkType (e : Exp, expectedType : Type) : Boolean =
        if (tipe (e) == NoType () || expectedType == NoType () ||
                tipe (e) == expectedType) {
            true
        } else {
            message (e, s"expected ${pretty (expectedType)}, found ${pretty (tipe (e))}")
            false
        }

    /**
     * The type of an expression.  Checks constituent names and types.  Uses
     * the env attribute to get the bound variables and their types.
     */
    val tipe : Exp => Type =
        attr {

            // A number is always of integer type
            case Num (_) =>
                IntType ()

            // An identifier is looked up in the environement of the current
            // expression.  If we find it, then we use the type that we find.
            // Otherwise it's an error.
            case e @ Var (x) =>
                (e->env).collectFirst {
                    case (y, t) if x == y => t
                }.getOrElse {
                    message (e, s"'$x' unknown")
                    NoType ()
                }

            // A lambda expression is a function from the type of its argument
            // to the type of the body expression
            case Lam (_, t, e) =>
                e->tipe
                if (t == NoType ())
                    NoType ()
                else
                    FunType (t, e->tipe)

            // For an application we first determine the type of the expression
            // being applied.  If it's a function whose argument type is the same
            // as the type of the argument in the application, then the type of
            // the application is the type of the body of the function.  If it's
            // a function but the argument types do not match, then it's an error.
            // If it's not a function then it's also an error.
            case App (e1, e2) =>
                e1->tipe match {
                    case FunType (t1, t2) =>
                        if (checkType (e2, t1))
                            t2
                        else
                            NoType ()
                    case NoType () =>
                        NoType ()
                    case _ =>
                        message (e1, "application of non-function")
                        NoType ()
                }

            // An operation must be applied to two integers and returns an
            // integer.
            case Opn (e1, op, e2) =>
                if (checkType (e1, IntType ()) && checkType (e2, IntType ()))
                    IntType ()
                else
                    NoType ()

            // A let returns the type of the body expression
            case Let (i, t, e1, e2) =>
                if (checkType (e1, t))
                    e2->tipe
                else
                    NoType ()

            // A parallel returns the type of the body expression
            case Letp (bs, e) =>
                e->tipe
        }

    /**
     * For a given variable reference, return the lambda node that binds it if
     * there is one, otherwise return None.
     */
    def lookup (name : Idn) : Exp => Option[Lam] =
        attr {
            // Inside a lambda expression the bound variable is now visible
            // in addition to everything that is visible from above.  If
            // this lambda expression binds the name we are looking for, then
            // return this node.
            case e @ Lam (x, t, _) if x == name => Some (e)

            // Nothing is visible at the root of the tree
            case e if e.isRoot                  => None


            // Other expressions do not bind new identifiers so they just
            // get their environment from their parent
            case e                              => e.parent[Exp]->lookup (name)
        }

    /**
     * Return whether the type of `e` is `expectedType` or unknown. If it
     * isn't either of these, also report an error.
     */
    def checkType2 (e : Exp, expectedType : Type) : Boolean =
        if (tipe2 (e) == NoType () || expectedType == NoType () ||
                tipe2 (e) == expectedType) {
            true
        } else {
            message (e, s"expected ${pretty (expectedType)}, found ${pretty (tipe2 (e))}")
            false
        }

    /**
     * The type of an expression.  Checks constituent names and types. Uses
     * the lookup attribute to get the lambda node that binds a name. For
     * other cases it behaves like tipe.
     */
    val tipe2 : Exp => Type =
        attr {

            // A number is always of integer type
            case Num (_) =>
                IntType ()

            // An identifier is looked up in the environement of the current
            // expression.  If we find it, then we use the type that we find.
            // Otherwise it's an error.
            case e @ Var (x) =>
                (e->lookup (x)) match {
                    case Some (Lam (_, t, _)) => t
                    case None =>
                        message (e, s"'$x' unknown")
                        NoType ()
                }

            // A lambda expression is a function from the type of its argument
            // to the type of the body expression
            case Lam (_, t, e) =>
                e->tipe2
                if (t == NoType ())
                    NoType ()
                else
                    FunType (t, e->tipe2)

            // For an application we first determine the type of the expression
            // being applied.  If it's a function whose argument type is the same
            // as the type of the argument in the application, then the type of
            // the application is the type of the body of the function.  If it's
            // a function but the argument types do not match, then it's an error.
            // If it's not a function then it's also an error.
            case App (e1, e2) =>
                e1->tipe2 match {
                    case FunType (t1, t2) =>
                        if (checkType2 (e2, t1))
                            t2
                        else
                            NoType ()
                    case NoType () =>
                        NoType ()
                    case _ =>
                        message (e1, "application of non-function")
                        NoType ()
                }

            // An operation must be applied to two integers and returns an
            // integer.
            case Opn (e1, op, e2) =>
                if (checkType2 (e1, IntType ()) && checkType2 (e2, IntType ()))
                    IntType ()
                else
                    NoType ()

            // A let returns the type of the body expression
            case Let (i, t, e1, e2) =>
                if (checkType2 (e1, t))
                    e2->tipe2
                else
                    NoType ()


            // A parallel returns the type of the body expression
            case Letp (bs, e) =>
                e->tipe2
        }

    /**
     * The declaration (if any) of an identifier use.
     */
    def decl : Var => Option[Lam] =
        attr {
            case e @ Var (x) => e->lookup (x)
        }

}
