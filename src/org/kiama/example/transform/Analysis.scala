/*
 * Analysis for transformation compiler.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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
package example.transform

/**
 * Operator priority resolution and name analysis for transformation compiler.
 * Transform the generic operator tree from the parser into one that
 * correctly represents the precedence of the operators.  Operators are
 * assumed to be left associative.
 */
object Analysis {

    import AST._
    import org.kiama.attribution.Attributable
    import org.kiama.attribution.Attribution._
    import org.kiama.util.Messaging._
    import scala.collection.immutable.HashMap

    def process (program : Program) : Exp = {

        lazy val prioenv : Program ==> Map[String,Int] =
            attr {
                case p => HashMap (p.ops : _*)
            }

        def prio (op : String) : Int =
            (program->prioenv) getOrElse (op, 0)

        lazy val op_tree : ExpR ==> Exp =
            attr {
                case BinExpR (_, _, e1) => e1->op_tree
                case e1 @ Factor (e)    =>
                    val (optor, opnd) = e1->ops
                    val (_, es) = eval_top (optor, null, e :: opnd)
                    es.head
            }

        type Stacks = (List[String], List[Exp])

        lazy val ops : ExpR ==> Stacks =
            childAttr {
                case e1 => {
                    case _ : Program             => (Nil, Nil)
                    case e0 @ BinExpR (e, op, _) =>
                        val (optor, opnd) = e0->ops
                        eval_top (optor, op, e :: opnd)
                }
            }

        def eval_top (optor : List[String], op : String, opnd : List[Exp]) : Stacks =
            optor match {
                case Nil                => (List (op), opnd)
                case top_op :: rest_ops =>
                    if (prio (top_op) < prio (op))
                        (op :: top_op :: rest_ops, opnd)
                    else {
                        val o1 :: o2 :: rest = opnd
                        eval_top (rest_ops, op, BinExp (o2, top_op, o1) :: rest)
                    }
            }

        /**
         * Report errors in an expression.  Currently only variables that
         * are used but not declared.  Multiple declarations of the same
         * variable are ok.
         */
        lazy val errors : Exp ==> Unit =
            attr {
                case BinExp (l, o, r) =>
                    l->errors; r->errors
                case e @ Var (s) if (e->lookup (s) == None) =>
                    message (e, s + " is not declared")
                case _ =>
            }

        /**
         * Lookup a name at a particular node, returning a Some value
         * containing the associated declaration or None if there no
         * such declaration.
         */
        lazy val lookup : String => Attributable ==> Option[VarDecl] =
            paramAttr {
                s => {
                    case p : Program => p.vars.find (_.name == s)
                    case e           => (e.parent)->lookup (s)
                }
            }

        /**
         * Version of op_tree that splices the new tree into the old.
         */
        implicit val ast : ExpR ==> Exp =
            tree {
                case e => e->op_tree
            }

        // Explicit forward to ast
        // program.expr->ast->errors

        // Implicit forward to ast
        program.expr->errors

        // Return the AST for future processing
        program.expr->ast

    }

}
