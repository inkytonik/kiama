/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2014 Anthony M Sloane, Macquarie University.
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
class SemanticAnalyser {

    import TransformTree._
    import org.kiama.attribution.Attribution._
    import org.kiama.util.Messaging.{collectmessages, message, Messages}
    import scala.collection.immutable.{HashMap, Seq}

    lazy val prioenv : Program => Map[String,Int] =
        attr (
            p => HashMap (p.ops : _*)
        )

    lazy val prio : String => TransformTree => Int =
        paramAttr (
            op => {
                case p : Program => (p->prioenv) getOrElse (op, 0)
                case e           => (e.parent[TransformTree])->prio (op)
            }
        )

    lazy val op_tree : ExpR => Exp =
        attr {
            case BinExpR (_, _, e1) =>
                e1->op_tree
            case e1 @ Factor (e)    =>
                val (optor, opnd) = e1->ops
                val (_, es) = e1->eval_top (optor, "", e +: opnd)
                es.head
        }

    type Stacks = (Seq[String], Seq[Exp])

    lazy val ops : ExpR => Stacks =
        childAttr {
            case e1 => {
                case _ : Program             =>
                    (Nil, Nil)
                case e0 @ BinExpR (e, op, _) =>
                    val (optor, opnd) = e0->ops
                    e0->eval_top (optor, op, e +: opnd)
            }
        }

    lazy val eval_top : ((Seq[String], String, Seq[Exp])) => ExpR => Stacks =
        paramAttr {
            case (Nil, op, opnd) => (
                _ =>
                    (Seq (op), opnd)
            )
            case (top_op :: rest_ops, op, opnd) => (
                e =>
                    if (e->prio (top_op) < e->prio (op))
                        (op :: top_op :: rest_ops, opnd)
                    else {
                        val o1 :: o2 :: rest = opnd
                        e->eval_top (rest_ops, op, BinExp (o2, top_op, o1) :: rest)
                    }
            )
        }

    /**
     * Lookup a name at a particular node, returning a Some value
     * containing the associated declaration or None if there no
     * such declaration.
     */
    lazy val lookup : String => TransformTree => Option[VarDecl] =
        paramAttr {
            s => {
                case p : Program => p.vars.find (_.name == s)
                case e           => e.parent[TransformTree]->lookup (s)
            }
        }

    /**
     * Version of op_tree that splices the new tree into the old.
     * Available as an implicit so that Exp attributes called on
     * ExpR are forwarded to op_tree.
     */
    implicit val ast : ExpR => Exp =
        tree (
            e => e->op_tree
        )

    /**
     * Report errors in an expression.  Currently only variables that
     * are used but not declared.  Multiple declarations of the same
     * variable are ok.
     */
    lazy val errors : TransformTree => Messages =
        attr { collectmessages {
            case e @ Var (s) if e->lookup (s) == None =>
                message (e, s"$s is not declared")
        }}

}
