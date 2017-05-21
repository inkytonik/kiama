/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.transform

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import TransformTree.TransformTree

/**
 * Operator priority resolution and name analysis for transformation compiler.
 * Transform the generic operator tree from the parser into one that
 * correctly represents the precedence of the operators.  Operators are
 * assumed to be left associative.
 */
class SemanticAnalyser(tree : TransformTree) extends Attribution {

    import TransformTree._
    import org.bitbucket.inkytonik.kiama.util.Messaging.{collectMessages, message, Messages}
    import scala.collection.immutable.HashMap

    lazy val prioenv : Program => Map[String, Int] =
        attr(
            p => HashMap(p.ops : _*)
        )

    lazy val prio : String => TransformNode => Int =
        paramAttr(
            op => {
                case tree.parent(p) =>
                    prio(op)(p)
                case p : Program =>
                    prioenv(p) getOrElse (op, 0)
            }
        )

    lazy val op_tree : ExpR => Exp =
        attr {
            case BinExpR(_, _, e1) =>
                op_tree(e1)
            case e1 @ Factor(e) =>
                val (optor, opnd) = ops(e1)
                val (_, es) = eval_top((optor, "", e +: opnd))(e1)
                es.head
        }

    type Stacks = (List[String], List[Exp])

    lazy val ops : ExpR => Stacks =
        {
            case tree.parent(e0 @ BinExpR(e, op, _)) =>
                val (optor, opnd) = ops(e0)
                eval_top((optor, op, e +: opnd))(e0)
            case _ =>
                (Nil, Nil)
        }

    lazy val eval_top : ((List[String], String, List[Exp])) => ExpR => Stacks =
        paramAttr {
            case (Nil, op, opnd) => (
                _ =>
                    (List(op), opnd)
            )
            case (top_op :: rest_ops, op, opnd) => (
                e =>
                    if (prio(top_op)(e) < prio(op)(e))
                        (op :: top_op :: rest_ops, opnd)
                    else {
                        val o1 :: o2 :: rest = opnd
                        eval_top((rest_ops, op, BinExp(o2, top_op, o1) :: rest))(e)
                    }
            )
        }

    /**
     * Lookup a name at a particular node, returning a Some value
     * containing the associated declaration or None if there no
     * such declaration.
     */
    lazy val lookup : String => TransformNode => Option[VarDecl] =
        paramAttr {
            s =>
                {
                    case tree.parent(p) =>
                        lookup(s)(p)
                    case p : Program =>
                        p.vars.find(_.name == s)
                }
        }

    /**
     * Version of op_tree that splices the new tree into the old.
     * Available as an implicit so that Exp attributes called on
     * ExpR are forwarded to op_tree.
     */
    implicit val ast : ExpR => Exp =
        op_tree

    /**
     * Report errors in an expression.  Currently only variables that
     * are used but not declared.  Multiple declarations of the same
     * variable are ok.
     */
    lazy val errors : Messages =
        collectMessages(tree) {
            case e @ Var(s) if lookup(s)(e) == None =>
                message(e, s"$s is not declared")
        }

}
