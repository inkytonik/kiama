/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
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

import LambdaTree.LambdaTree
import org.kiama.attribution.Attribution
import org.kiama.util.Messaging

/**
 * Analyses for typed lambda calculus expressions.  A simple free variable
 * analysis plus name and type analysis.  There are two versions of the
 * latter here: one (tipe) that constructs an explicit environment separate
 * from the AST, and one (tipe2) that represents names by references to the
 * nodes of their binding lambda expressions.
 */
class Analyser (tree : LambdaTree) extends Attribution {

    import LambdaTree._
    import PrettyPrinter._
    import org.kiama.util.Messaging.{check, collectmessages, message, Messages}
    import scala.collection.immutable.Seq

    /**
     * The semantic error messages for the tree. This one uses the `tipe`
     * attribute.
     */
    lazy val errors : Messages =
        collectmessages (tree) {
            case e : Exp =>
                checkType (e, tipe) ++
                check (e) {
                    case App (e1, e2) =>
                        check (tipe (e1)) {
                            case _ : IntType =>
                                message (e1, "application of non-function")
                        }
                    case Var (x) =>
                        message (e, s"'$x' unknown", tipe (e) == UnknownType ())
                }
        }

    /**
     * The semantic error messages for the tree. This one uses the `tipe2`
     * attribute.
     */
    lazy val errors2 : Messages =
        collectmessages (tree) {
            case e : Exp =>
                checkType (e, tipe2) ++
                check (e) {
                    case App (e1, e2) =>
                        check (tipe2 (e1)) {
                            case _ : IntType =>
                                message (e1, "application of non-function")
                        }
                    case Var (x) =>
                        message (e, s"'$x' unknown", tipe2 (e) == UnknownType ())
                }
        }

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

            // Inside a lambda expression the bound variable is now visible
            // in addition to everything that is visible from above. Note
            // that an inner declaration of a var hides an outer declaration
            // of the same var since we add inner bindings at the beginning
            // of the env and we search the env list below in tipe from
            // beginning to end
            case tree.parent (p @ Lam (x, t, _)) =>
                (x,t) +: env (p)

            // Other expressions do not bind new identifiers so they just
            // get their environment from their parent
            case tree.parent (p : Exp) =>
                env (p)

            // Nothing is visible at the root of the tree
            case _ =>
                Seq ()

        }

    /**
     * Check that the type of `e` is its expected type or unknown. If not,
     * return an error. `tipe` is used to obtain the type.
     */
    def checkType (e : Exp, tipe : Exp => Type) : Messages = {
        val expectedType = exptipe (e)
        message (e, s"expected ${pretty (expectedType)}, found ${pretty (tipe (e))}",
                 tipe (e) != NoType () && tipe (e) != UnknownType () &&
                     expectedType != NoType () && tipe (e) != expectedType)
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
                env (e).collectFirst {
                    case (y, t) if x == y => t
                }.getOrElse {
                    UnknownType ()
                }

            // A lambda expression is a function from the type of its argument
            // to the type of the body expression
            case Lam (_, t, e) =>
                tipe (e)
                if (t == NoType ())
                    NoType ()
                else
                    FunType (t, tipe (e))

            // For an application we first determine the type of the expression
            // being applied.  If it's a function then the application has type
            // of that function's return type. If it's not a function then any
            // type is allowed. We check separately that only functions are
            // applied.
            case App (e1, e2) =>
                tipe (e1) match {
                    case FunType (t1, t2) =>
                        if (tipe (e1) == t1)
                            t2
                        else
                            NoType ()
                    case _ =>
                        NoType ()
                }

            // An operation must be applied to two integers and returns an
            // integer.
            case Opn (e1, op, e2) =>
                if ((tipe (e1) == IntType ()) && (tipe (e2) == IntType ()))
                    IntType ()
                else
                    NoType ()

            // A let returns the type of the body expression
            case Let (i, t, e1, e2) =>
                if (tipe (e1) == t)
                    tipe (e2)
                else
                    NoType ()

            // A parallel returns the type of the body expression
            case Letp (bs, e) =>
                tipe (e)
        }

    /**
     * The expected type of an expression.
     */
    val exptipe : Exp => Type =
        attr {

            // An applied expression is allowed to be anything. We check
            // elsewhere that it's a function.
            case tree.parent.pair (e, App (e1, _)) if e eq e1 =>
                NoType ()

            // An argument is expected to be of the function's input type
            case tree.parent.pair (e, App (e1, e2)) if e eq e2 =>
                tipe (e1) match {
                    case FunType (t1, _) =>
                        t1
                    case _ =>
                        NoType ()
                }

            // The type of a let-bound expression must match the declared type
            case tree.parent.pair (e, Let (_, t, e1, _)) if e eq e1 =>
                t

            // The type of let body must match the context of the let
            case tree.parent.pair (e, p @ Let (_, t, _, e2)) if e eq e2 =>
                exptipe (p)

            // The operands of an operation should be integers
            case tree.parent (Opn (_, _, _)) =>
                IntType ()

            // Other expressions are allowed to be anything
            case _ =>
                NoType ()

        }

    /**
     * For a given variable reference, return the lambda node that binds it if
     * there is one, otherwise return None.
     */
    def lookup (name : Idn) : ExpNode => Option[Lam] =
        attr {
            // Inside a lambda expression the bound variable is now visible
            // in addition to everything that is visible from above.  If
            // this lambda expression binds the name we are looking for, then
            // return this node.
            case e @ Lam (x, t, _) if x == name =>
                Some (e)

            // Other nested expressions do not bind new identifiers so they just
            // get their environment from their parent
            case tree.parent (p) =>
                lookup (name) (p)

            // Nothing is visible at the root of the tree
            case _ =>
                None
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
                lookup (x) (e) match {
                    case Some (Lam (_, t, _)) =>
                        t
                    case None =>
                        UnknownType ()
                }

            // A lambda expression is a function from the type of its argument
            // to the type of the body expression
            case Lam (_, t, e) =>
                tipe2 (e)
                if (t == NoType ())
                    NoType ()
                else
                    FunType (t, tipe2 (e))

            // For an application we first determine the type of the expression
            // being applied.  If it's a function then the application has type
            // of that function's return type. If it's not a function then any
            // type is allowed. We check separately that only functions are
            // applied.
            case App (e1, e2) =>
                tipe2 (e1) match {
                    case FunType (t1, t2) =>
                        if (tipe2 (e2) == t1)
                            t2
                        else
                            NoType ()
                    case _ =>
                        NoType ()
                }

            // An operation must be applied to two integers and returns an
            // integer.
            case Opn (e1, op, e2) =>
                if ((tipe (e1) == IntType ()) && (tipe (e2) == IntType ()))
                    IntType ()
                else
                    NoType ()

            // A let returns the type of the body expression
            case Let (i, t, e1, e2) =>
                if (tipe2 (e1) == t)
                    tipe2 (e2)
                else
                    NoType ()

            // A parallel returns the type of the body expression
            case Letp (bs, e) =>
                tipe2 (e)
        }

    /**
     * The declaration (if any) of an identifier use.
     */
    def decl : Var => Option[Lam] =
        attr {
            case e @ Var (x) => lookup (x) (e)
        }

}
