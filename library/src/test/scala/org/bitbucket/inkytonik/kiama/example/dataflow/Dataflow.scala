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
package example.dataflow

import DataflowTree._
import org.bitbucket.inkytonik.kiama.attribution.Attribution

class Dataflow(val tree : DataflowTree) extends Attribution {

    // Control flow

    /**
     * Control flow successor relation.
     */
    val succ : Stm => Set[Stm] =
        dynAttr {
            case If(_, s1, s2) =>
                Set(s1, s2)
            case t @ While(_, s) =>
                following(t) + s
            case Return(_) =>
                Set()
            case Block(s :: _) =>
                Set(s)
            case s =>
                following(s)
        }

    /**
     * Control flow default successor relation.
     */
    val following : Stm => Set[Stm] =
        dynAttr {
            case tree.parent(t : If) =>
                following(t)
            case tree.parent(t : While) =>
                Set(t)
            case tree.parent.pair(tree.next(n), _ : Block) =>
                Set(n)
            case tree.parent(b : Block) =>
                following(b)
            case _ =>
                Set()
        }

    // Variable use and definition

    /**
     * Variable uses.
     */
    val uses : Stm => Set[Var] =
        dynAttr {
            case If(v, _, _)  => Set(v)
            case While(v, _)  => Set(v)
            case Assign(_, v) => Set(v)
            case Return(v)    => Set(v)
            case _            => Set()
        }

    /**
     * Variable definitions.
     */
    val defines : Stm => Set[Var] =
        dynAttr {
            case Assign(v, _) => Set(v)
            case _            => Set()
        }

    // Variable liveness

    /**
     * Variables "live" into a statement.
     */
    val in : Stm => Set[Var] =
        circular(Set[Var]())(
            // Optimisation to not include vars used to calculate v
            // if v is not live in the following.
            // case s @ Assign (v, _) if ! (out (s) contains v) =>
            //    out (s)
            s => uses(s) ++ (out(s) -- defines(s))
        )

    /**
     * Variables "live" out of a statement.
     */
    val out : Stm => Set[Var] =
        circular(Set[Var]())(
            s => succ(s) flatMap (in)
        )

}
